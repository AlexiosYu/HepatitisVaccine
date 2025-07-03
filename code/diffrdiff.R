rm(list = ls());gc()
source("./code/pred_hyperparameter.R")
source("./code/parallel_extract_function_WGS4326.R")
source("./code/model_linkage_WGS4326.R")
source("./code/spatial_extraction.R")
source("./code/ml_predcsv.R")
source("./code/rake_functions.R")
source("./code/rake_poly_function.R")
model_names <- c("las","gam","rf")
selected_year_index <- c(2010:2022)-2010+1
hep1 <- read.csv("./data/hep1_datat111.csv")
hep1 <- hep1[which(!is.na(hep1$year)),]
hep3 <- read.csv("./data/hep3_datat.csv")[,2:5]
hep3 <- hep3[which(!is.na(hep3$year)),]
mat <- data_frame()
for (i in c(2010,2022,2011:2021)-2009) {
  time_2 <- Sys.time()
  load(sprintf("./Rdata/%s/pred_1000_%s.Rdata","D1",(selected_year_index[i]  + 2009)))
  shp_10 <- read_sf(sprintf("./shape/%s/5km_%s.shp","D1",(selected_year_index[i]  + 2009)))
  yr <- selected_year_index[i]
  pred <- list_1000$pred
  pred_con <- list_1000$pred_con
  adjusted_pred_1 <- data_frame()
  adjusted_tibs_1 <- data_frame()
  
  for (j in unique(pred_con$con_id)) {
    gbdval <- hep1$mean[which((hep1$con_id==j)&(hep1$year==yr+2009))]
    nlen <- which(shp_10$con_id==j)
    vals <- pred[nlen,]
    if(j %in% hep1$con_id){
      
      weights <- shp_10$population[nlen]
      K <- SimpleFindK(gbdval, vals, weights)
      adjusted_vals <- adjust_matrix(K, vals)
      
      
      
    }else{
      adjusted_vals <- vals
    }
    adjusted_pred_1 <- rbind(adjusted_pred_1,adjusted_vals)
    tibs <- cbind(shp_10[nlen,] %>% st_drop_geometry(),shp_10[nlen,] %>% st_coordinates())
    adjusted_tibs_1 <- rbind(adjusted_tibs_1,tibs)
  }
  
  
  load(sprintf("./Rdata/%s/pred_1000_%s.Rdata","D3",(selected_year_index[i]  + 2009)))
  shp_10 <- read_sf(sprintf("./shape/%s/5km_%s.shp","D3",(selected_year_index[i]  + 2009)))
  yr <- selected_year_index[i]
  pred <- list_1000$pred
  pred_con <- list_1000$pred_con
  adjusted_pred_3 <- data_frame()
  adjusted_tibs_3 <- data_frame()
  
  for (j in unique(pred_con$con_id)) {
    gbdval <- hep3$mean[which((hep3$con_id==j)&(hep3$year==yr+2009))]
    nlen <- which(shp_10$con_id==j)
    vals <- pred[nlen,]
    if(j %in% hep3$con_id){
      
      weights <- shp_10$population[nlen]
      K <- SimpleFindK(gbdval, vals, weights)
      adjusted_vals <- adjust_matrix(K, vals)
      
      
      
    }else{
      adjusted_vals <- vals
    }
    adjusted_pred_3 <- rbind(adjusted_pred_3,adjusted_vals)
    tibs <- cbind(shp_10[nlen,] %>% st_drop_geometry(),shp_10[nlen,] %>% st_coordinates())
    adjusted_tibs_3 <- rbind(adjusted_tibs_3,tibs)
  }
  adpop <- adjusted_tibs_3$population %>% unlist()
  diff <- adjusted_pred_1-adjusted_pred_3
  r_diff <- (adjusted_pred_1-adjusted_pred_3)/adjusted_pred_1
  pop_pred <- diff*adpop
  pp1 <- adpop*adjusted_pred_1
  cpd <- colSums(pop_pred)
  pop_1000 <- cpd/colSums(pp1)

  write.csv(pop_1000,sprintf("./shape/rake/%s/%s_%s%s","diff","all",selected_year_index[i] + 2009,".csv"),row.names = F)
  write.csv(cpd,sprintf("./shape/rake/%s/%s_diff_%s%s","diff","all",selected_year_index[i] + 2009,".csv"),row.names = F)
  
  cl<-makeForkCluster(5)
  
  time1 <- Sys.time()
  cat(paste(time1,"start"))
  cat("\n")
  xbar = parApply(cl,r_diff,MARGIN=1,mean,na.rm=T)
  time2 <- Sys.time()
  cat(paste(time2,"mean"))
  cat("\n")
  s = parApply(cl,r_diff,MARGIN=1,sd,na.rm=T)
  time3 <- Sys.time()
  cat(paste(time3,"sd"))
  cat("\n")
  qt <- parApply(cl,r_diff, MARGIN=1, function(x) quantile(x,c(0.025,0.975),na.rm=T)) %>% t() %>% as.data.frame()
  time4 <- Sys.time()
  cat(paste(time4,"qt"))
  cat("\n")
  df.final <- data.frame(adjusted_tibs_3,
                         mean = xbar, 
                         sd = s,
                         low = qt$`2.5%`, 
                         up = qt$`97.5%`)
  time5 <- Sys.time()
  cat(paste(time5,"df"))
  cat("\n")
  cat("\n")
  result_shp <- st_as_sf(df.final, coords = c(x = "X", y = "Y"),crs=st_crs(32634))
  stopCluster(cl)
  
  ncluster <- 5
  pred_dis <- rake_poly_function(ncluster, r_diff,adjusted_tibs_3, c("con_id", "pro_id", "dis_id"),yr)
  gc()
  pred_pro <- rake_poly_function(ncluster, r_diff,adjusted_tibs_3, c("con_id", "pro_id"),yr)
  gc()
  pred_con <- rake_poly_function(ncluster, r_diff,adjusted_tibs_3, c("con_id"),yr)
  gc()
  time_3 <- Sys.time()
  sprintf("Time spent on process 2: \n") %>% cat()
  print(time_3-time1)
  write_name <- sapply(1:4, function(j){
    sprintf("./shape/rake/%s/rdiff_%s_%s%s","diff",c("5km","dis","pro","con")[j],selected_year_index[i] + 2009,c(".shp",".csv",".csv",".csv")[j])
  })
  
  write_sf(result_shp,dsn=write_name[1])
  write_csv(pred_dis,file = write_name[2])
  write_csv(pred_pro,file = write_name[3])
  write_csv(pred_con,file = write_name[4])
  time_n <- Sys.time()
  sprintf("Time totally:") %>% cat()
  print(time_n-time1)
  
  
  cl<-makeForkCluster(5)
  
  time1 <- Sys.time()
  cat(paste(time1,"start"))
  cat("\n")
  xbar = parApply(cl,diff,MARGIN=1,mean,na.rm=T)
  time2 <- Sys.time()
  cat(paste(time2,"mean"))
  cat("\n")
  s = parApply(cl,diff,MARGIN=1,sd,na.rm=T)
  time3 <- Sys.time()
  cat(paste(time3,"sd"))
  cat("\n")
  qt <- parApply(cl,diff, MARGIN=1, function(x) quantile(x,c(0.025,0.975),na.rm=T)) %>% t() %>% as.data.frame()
  time4 <- Sys.time()
  cat(paste(time4,"qt"))
  cat("\n")
  df.final <- data.frame(adjusted_tibs_3,
                         mean = xbar, 
                         sd = s,
                         low = qt$`2.5%`, 
                         up = qt$`97.5%`)
  time5 <- Sys.time()
  cat(paste(time5,"df"))
  cat("\n")
  cat("\n")
  result_shp <- st_as_sf(df.final, coords = c(x = "X", y = "Y"),crs=st_crs(32634))
  stopCluster(cl)
  
  ncluster <- 5
  pred_dis <- rake_poly_function(ncluster, diff,adjusted_tibs_3, c("con_id", "pro_id", "dis_id"),yr)
  gc()
  pred_pro <- rake_poly_function(ncluster, diff,adjusted_tibs_3, c("con_id", "pro_id"),yr)
  gc()
  pred_con <- rake_poly_function(ncluster, diff,adjusted_tibs_3, c("con_id"),yr)
  gc()
  time_3 <- Sys.time()
  sprintf("Time spent on process 2: \n") %>% cat()
  print(time_3-time1)
  write_name <- sapply(1:4, function(j){
    sprintf("./shape/rake/%s/diff_%s_%s%s","diff",c("5km","dis","pro","con")[j],selected_year_index[i] + 2009,c(".shp",".csv",".csv",".csv")[j])
  })
  
  write_sf(result_shp,dsn=write_name[1])
  write_csv(pred_dis,file = write_name[2])
  write_csv(pred_pro,file = write_name[3])
  write_csv(pred_con,file = write_name[4])
  time_n <- Sys.time()
  sprintf("Time totally:") %>% cat()
  print(time_n-time1)

  }
