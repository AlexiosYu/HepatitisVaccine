rm(list = ls());gc()
library(readr)
library(tidyr)
library(dplyr)
hep3_rake <- read_csv("./data/hep3_datat.csv")[,3:5]
poly_maker <- function(result_mat,table_column,grouping_vars){
  grouping_vars <- c(grouping_vars,"B")
  cl <- parallel::makeForkCluster(20)
  
  # group_by function to calculate the probability 
  result_matr <- parLapplyLB(cl, 1:1000, function(i){
    i_pred <- result_mat[,i] %>% as.data.frame()
    colnames(i_pred) <- "a"
    matr <- cbind(table_column,i_pred) %>% 
      group_by(!!!rlang::syms(grouping_vars)) %>% 
      dplyr::summarise(p = sum(a*population,na.rm = T)/sum(population,na.rm = T)) %>% 
      ungroup(!!!rlang::syms(grouping_vars))
    colnames(matr)[ncol(matr)] <- sprintf("rake_%d",i)
    return(matr)
  })  %>% 
    reduce(.,left_join,by = grouping_vars)
  
  cols_with_p <- result_matr[, grepl("^rake_", colnames(result_matr))]
  col_without_p <- result_matr[, !grepl("^rake_", colnames(result_matr))]
  
  # 计算统计量
  xbar <- parallel::parApply(cl, cols_with_p, MARGIN = 1, FUN = mean, na.rm = TRUE)
  time1 <- Sys.time()
  cat(paste(time1,"x"))
  cat("\n")
  s <- parallel::parApply(cl, cols_with_p, MARGIN = 1, FUN = sd, na.rm = TRUE)
  time2 <- Sys.time()
  cat(paste(time2,"\t"))
  cat("\n")
  qt <- parallel::parApply(cl, cols_with_p, MARGIN = 1, FUN = function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE)) %>%
    t() %>%
    as.data.frame()
  time3 <- Sys.time()
  cat(time3)
  cat("\n")
  # 关闭集群
  parallel::stopCluster(cl)
  
  # 构建最终数据框
  df_final <- data.frame(
    col_without_p,
    mean = xbar,
    sd = s,
    low = qt$`2.5%`,
    up = qt$`97.5%`
  )
  return(list(
    pred_result = df_final,
    pred_result_1000 = result_matr
  ))
}
for (i in 11:13) {
  load(sprintf("./Rdata/%s/pred_1000_%s.Rdata","D3",(i  + 2009)))
  sf_dat <- read_sf(sprintf("~/Yu/project/shape/D3/5km_%s.shp",(i  + 2009)))
  coords <- sf_dat %>% st_coordinates() %>% as.data.frame()
  colnames(coords) <- c("LONNUM", "LATNUM")
  table_column <- cbind(sf_dat %>% st_drop_geometry(),coords)%>% dplyr::select(ID,LONNUM,LATNUM,con_id,pro_id,dis_id,B,population)
  
  hep3_year <- hep3_rake %>% filter(year==i+2009)
  pred <- list_1000$pred
  
  pred_con <- list_1000$pred_con
  rake_1000 <- left_join(hep3_year,pred_con)
  rc <- hep3_year$con_id
  rake_1000 <- rake_1000 %>%
    group_by(con_id) %>% 
    summarise(across(
      starts_with("p_"), 
      ~ mean / .x, 
      .names = "rake_{sub('p_', '', .col)}"
    )) %>% 
    ungroup()
  rc_53 <- c(53,rep(1,1000)) %>% t()
  colnames(rc_53) <- colnames(rake_1000)
  rake_1000 <- rbind(rake_1000,rc_53)
  rake_info <- left_join(table_column,rake_1000) %>% select(-ID,-LONNUM,-LATNUM,-con_id,-pro_id,-dis_id,-B,-population)
  result_mat <- pred * rake_info
  
  
  cl<-makeForkCluster(20)
  
  time1 <- Sys.time()
  cat(paste(time1,"x"))
  cat("\n")
  xbar = parApply(cl,result_mat,MARGIN=1,mean,na.rm=T)
  time2 <- Sys.time()
  cat(time2)
  cat("\n")
  s = parApply(cl,result_mat,MARGIN=1,sd,na.rm=T)
  time3 <- Sys.time()
  cat(time3)
  cat("\n")
  qt <- parApply(cl,result_mat, MARGIN=1, function(x) quantile(x,c(0.025,0.975),na.rm=T)) %>% t() %>% as.data.frame()
  df.final <- data.frame(table_column,
                         mean = xbar, 
                         sd = s,
                         low = qt$`2.5%`, 
                         up = qt$`97.5%`)
  time4 <- Sys.time()
  cat(time4)
  cat("\n")
  result_shp <- st_as_sf(df.final, coords = c(x = "LONNUM", y = "LATNUM"),crs=st_crs(32634)) 
  stopCluster(cl)
  
  
  
  write_sf(result_shp,sprintf("~/Yu/project/shape/rake/D3/5km_%s.shp",(i  + 2009)))
  
  
  result_matx <- cbind(table_column,result_mat)
  af <-  sapply(1:1000,function(x) {
    sum(result_mat[,x]*table_column$population)/sum(table_column$population)
  })
  afrate <- data.frame(year=i+2009,
                       mean = mean(af), 
                       sd = sd(af),
                       low = quantile(af,0.025,na.rm=T), 
                       up = quantile(af,0.975,na.rm=T))
  write_csv(afrate,file=sprintf("~/Yu/project/shape/rake/D3/af_%s.csv",(i  + 2009)))
  pred_dis_list <- poly_maker(result_mat,table_column, c("con_id", "pro_id", "dis_id"))
  gc()
  pred_pro_list <- poly_maker(result_mat,table_column, c("con_id", "pro_id"))
  gc()
  pred_con_list <- poly_maker(result_mat,table_column,c("con_id"))
  gc()
  write_csv(pred_con_list$pred_result,file=sprintf("~/Yu/project/shape/rake/D3/con_%s.csv",(i  + 2009)))
  write_csv(pred_pro_list$pred_result,file=sprintf("~/Yu/project/shape/rake/D3/pro_%s.csv",(i  + 2009)))
  write_csv(pred_dis_list$pred_result,file=sprintf("~/Yu/project/shape/rake/D3/dis_%s.csv",(i  + 2009)))
}
