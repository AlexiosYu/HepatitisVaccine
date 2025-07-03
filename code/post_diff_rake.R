rm(list = ls());gc()
source("./code/pred_hyperparameter.R")
source("./code/parallel_extract_function_WGS4326.R")
source("./code/model_linkage_WGS4326.R")
source("./code/spatial_extraction.R")
source("./code/ml_predcsv.R")
source("./code/rake_functions.R")
source("./code/rake_poly_function.R")
source("./code/rake_poly_function_halfway.R")
al_list <- list()
tib_list <- list()
index <- c(1,6,10,11,13)
hep3 <- read.csv("./data/hep3_datat.csv")[,2:5]
hep3 <- hep3[which(!is.na(hep3$year)),]
for (i in 1:5) {
  time1 <- Sys.time()
  print(time1)
  yr <- index[i]
  load(sprintf("./Rdata/%s/pred_1000_%s.Rdata","D3",(yr  + 2009)))
  shp_10 <- read_sf(sprintf("./shape/%s/5km_%s.shp","D3",(yr  + 2009)))

  pred <- list_1000$pred
  pred_con <- list_1000$pred_con
  adjusted_pred <- data_frame()
  adjusted_tibs <- data_frame()
  
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
    adjusted_pred <- rbind(adjusted_pred,adjusted_vals)
    tibs <- cbind(shp_10[nlen,] %>% st_drop_geometry(),shp_10[nlen,] %>% st_coordinates())
    adjusted_tibs <- rbind(adjusted_tibs,tibs)
  }
  al_list[[i]] <- adjusted_pred
  tib_list[[i]] <- adjusted_tibs
  time2 <- Sys.time()
  print(time2)
  print(time2-time1)
  rm(adjusted_tibs,adjusted_pred,list_1000)
  gc()
}
save(list = c("al_list","tib_list"),file = "diff_all.Rdata")
system.time({
  ####5km tiff####
  p2221 <- al_list[[5]] - al_list[[4]]
  p2120 <- al_list[[4]] - al_list[[3]]
  p2015 <- al_list[[3]] - al_list[[2]]
  p1510 <- al_list[[2]] - al_list[[1]]
  
  load("~/Yu/project/Rdata/sorted_ids.Rdata")
  mat5_diff <- matrix(NA,nrow = nrow(p1510),ncol = 4) %>% as.data.frame()
  colnames(mat5_diff) <- c(
    paste0("diff_",1:4,sep="")
  )
  cl<-makeForkCluster(5)
  
  mat5_diff$diff_1 <- parApply(cl,p1510,MARGIN=1,mean)
  mat5_diff$diff_2 <- parApply(cl,p2015,MARGIN=1,mean)
  mat5_diff$diff_3 <- parApply(cl,p2120,MARGIN=1,mean)
  mat5_diff$diff_4 <- parApply(cl,p2221,MARGIN=1,mean)
  stopCluster(cl)
  rm(p1510,p2015,p2120,p2221)
  gc()

  df.final <- data.frame(tib_list[1],
                         mat5_diff)
  
  geo_sf <- df.final %>% st_as_sf(coords = c(x = "X", y = "Y"),crs=st_crs(32634))
  st_geometry(geo_sf) <-  "geometry"
  write_sf(geo_sf,"./shape/5km_diff_rake.shp")
  rm(geo_sf,mat5_diff,sorted_ids)
})



load("diff_all.Rdata")
con_all <- list()
pro_all <- list()
dis_all <- list()
for (i in 1:5) {
  time1 <- Sys.time()
  print(time1)
  yr <- index[i]
  
  adjusted_pred <- al_list[[i]]
  adjusted_tibs <- tib_list[[i]]
  ncluster <- 2
  pred_dis <- rake_poly_function_halfway(ncluster, adjusted_pred,adjusted_tibs, c("con_id", "pro_id", "dis_id"),yr)
  gc()
  pred_pro <- rake_poly_function_halfway(ncluster, adjusted_pred,adjusted_tibs, c("con_id", "pro_id"),yr)
  gc()
  pred_con <- rake_poly_function_halfway(ncluster, adjusted_pred,adjusted_tibs, c("con_id"),yr)
  gc()
  
  dis_all[[i]] <- pred_dis
  pro_all[[i]] <- pred_pro
  con_all[[i]] <- pred_con
  time2 <- Sys.time()
  print(time2)
  print(time2-time1)
  gc()
}




system.time({
  ####dis tiff####
  p2221 <- con_all[[5]]$pred-con_all[[4]]$pred
  p2120 <- con_all[[4]]$pred-con_all[[3]]$pred
  p2015 <- con_all[[3]]$pred-con_all[[2]]$pred
  p1510 <- con_all[[2]]$pred-con_all[[1]]$pred
  
  mat5_diff <- matrix(NA,nrow = nrow(p1510),ncol = 4) %>% as.data.frame()
  colnames(mat5_diff) <- c(
    paste0("diff_",1:4,sep="")
  )
  cl<-makeForkCluster(5)
  
  mat5_diff$diff_1 <- parApply(cl,p1510,MARGIN=1,mean)
  mat5_diff$diff_2 <- parApply(cl,p2015,MARGIN=1,mean)
  mat5_diff$diff_3 <- parApply(cl,p2120,MARGIN=1,mean)
  mat5_diff$diff_4 <- parApply(cl,p2221,MARGIN=1,mean)
  stopCluster(cl)
  rm(p1510,p2015,p2120,p2221)
  gc()
  
  mat5_diff <- cbind(con_all[[1]]$tib,mat5_diff)
  write.csv(mat5_diff,"./shape/con_diff_rake.csv",col.names = T,row.names = F)
})
system.time({
  ####pro tiff####
  p2221 <- pro_all[[5]]$pred-pro_all[[4]]$pred
  p2120 <- pro_all[[4]]$pred-pro_all[[3]]$pred
  p2015 <- pro_all[[3]]$pred-pro_all[[2]]$pred
  p1510 <- pro_all[[2]]$pred-pro_all[[1]]$pred
  
  mat5_diff <- matrix(NA,nrow = nrow(p1510),ncol = 4) %>% as.data.frame()
  colnames(mat5_diff) <- c(
    paste0("diff_",1:4,sep="")
  )
  cl<-makeForkCluster(5)
  
  mat5_diff$diff_1 <- parApply(cl,p1510,MARGIN=1,mean)
  mat5_diff$diff_2 <- parApply(cl,p2015,MARGIN=1,mean)
  mat5_diff$diff_3 <- parApply(cl,p2120,MARGIN=1,mean)
  mat5_diff$diff_4 <- parApply(cl,p2221,MARGIN=1,mean)
  stopCluster(cl)
  rm(p1510,p2015,p2120,p2221)
  gc()
  
  mat5_diff <- cbind(pro_all[[1]]$tib,mat5_diff)
  write.csv(mat5_diff,"./shape/pro_diff_rake.csv",col.names = T,row.names = F)
})
system.time({
  ####dis tiff####
  p2221 <- dis_all[[5]]$pred-dis_all[[4]]$pred
  p2120 <- dis_all[[4]]$pred-dis_all[[3]]$pred
  p2015 <- dis_all[[3]]$pred-dis_all[[2]]$pred
  p1510 <- dis_all[[2]]$pred-dis_all[[1]]$pred
  
  mat5_diff <- matrix(NA,nrow = nrow(p1510),ncol = 4) %>% as.data.frame()
  colnames(mat5_diff) <- c(
    paste0("diff_",1:4,sep="")
  )
  cl<-makeForkCluster(5)
  
  mat5_diff$diff_1 <- parApply(cl,p1510,MARGIN=1,mean)
  mat5_diff$diff_2 <- parApply(cl,p2015,MARGIN=1,mean)
  mat5_diff$diff_3 <- parApply(cl,p2120,MARGIN=1,mean)
  mat5_diff$diff_4 <- parApply(cl,p2221,MARGIN=1,mean)
  stopCluster(cl)
  rm(p1510,p2015,p2120,p2221)
  gc()
  
  mat5_diff <- cbind(dis_all[[1]]$tib,mat5_diff)
  write.csv(mat5_diff,"./shape/dis_diff_rake.csv",col.names = T,row.names = F)
})
