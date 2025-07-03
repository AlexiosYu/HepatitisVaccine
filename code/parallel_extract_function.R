para_fine <- function(ncluster,area_extraction){
  time0 <- Sys.time()
  cat(paste(time0,"initializing"))
  cl<-makeForkCluster(ncluster)
  
  pred <- area_extraction$pred
  time1 <- Sys.time()
  cat(paste(time1,"x"))
  cat("\n")
  xbar = parApply(cl,pred,MARGIN=1,mean,na.rm=T)
  time2 <- Sys.time()
  cat(time2)
  cat("\n")
  s = parApply(cl,pred,MARGIN=1,sd,na.rm=T)
  time3 <- Sys.time()
  cat(time3)
  cat("\n")
  qt <- parApply(cl,pred, MARGIN=1, function(x) quantile(x,c(0.025,0.975),na.rm=T)) %>% t() %>% as.data.frame()
  table_property_column <- area_extraction$table_property_column
  df.final <- data.frame(table_property_column,
                         mean = xbar, 
                         sd = s,
                         low = qt$`2.5%`, 
                         up = qt$`97.5%`)
  time4 <- Sys.time()
  cat(time4)
  cat("\n")
  result_shp <- st_as_sf(df.final, coords = c(x = "X", y = "Y"),crs=st_crs(32634))
  stopCluster(cl)
  return(result_shp)
}

para_poly <- function(ncluster, area_extraction, grouping_vars,selected_year_index) {
  # Extracting the necessary data
  title_pred <- area_extraction$table_property_column %>%
    as.data.frame(stringsAsFactors = FALSE)
  pred <- area_extraction$pred_population %>%
    as.data.frame(stringsAsFactors = FALSE)
  grouping_vars <- c(grouping_vars)
  time0 <- Sys.time()
  cat(paste(time0,"initializing"))
  cat("\n")
  # Making fork cluster for parallel calculation
  cl <- parallel::makeForkCluster(ncluster)
  grouping_vars <- c(grouping_vars,"B")
  # group_by function to calculate the probability 
  result_matr <- parLapplyLB (cl, 1:1000, function(i){
    i_pred <- pred[,i] %>% as.data.frame()
    colnames(i_pred) <- "a"
    matr <- cbind(title_pred,i_pred) %>% 
      group_by(!!!rlang::syms(grouping_vars)) %>% 
      dplyr::summarise(p = sum(a,na.rm = T)/sum(population,na.rm = T)) %>% 
      ungroup(!!!rlang::syms(grouping_vars))
    colnames(matr)[ncol(matr)] <- sprintf("p_%d",i)
    return(matr)
  })  %>% 
    reduce(.,left_join,by = grouping_vars)
  
  # 分离含有 "p_" 的列和其他列
  cols_with_p <- result_matr[, grepl("^p_", colnames(result_matr))]
  col_without_p <- result_matr[, !grepl("^p_", colnames(result_matr))]
  
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
