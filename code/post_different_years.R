rm(list = ls())
load("./Rdata/D3/pred_1000_2022.Rdata")
l22 <- list_1000

load("./Rdata/D3/pred_1000_2021.Rdata")
l21 <- list_1000

load("./Rdata/D3/pred_1000_2020.Rdata")
l20 <- list_1000

load("./Rdata/D3/pred_1000_2019.Rdata")
l19 <- list_1000

load("./Rdata/D3/pred_1000_2015.Rdata")
l15 <- list_1000

load("./Rdata/D3/pred_1000_2010.Rdata")
l10 <- list_1000

rm(list_1000)

system.time({
  ####5km tiff####
  p2221 <- l22[[1]] - l21[[1]]
  p2120 <- l21[[1]] - l20[[1]]
  p2015 <- l20[[1]] - l15[[1]]
  p1510 <- l15[[1]] - l10[[1]]
  
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
  load("~/Yu/project/Rdata/sorted_ids.Rdata")
  df.final <- data.frame(sorted_ids,
                         mat5_diff)
  
  geo_sf <- df.final %>% st_as_sf(coords = c(x = "LONNUM", y = "LATNUM"),crs=st_crs(4236)) %>% st_transform(32634)
  st_geometry(geo_sf) <-  "geometry"
  write_sf(geo_sf,"./shape/5km_diff.shp")
  rm(geo_sf,mat5_diff,sorted_ids)
})




system.time({
  ####dis tiff####
  p2221 <- l22[[2]][,5:1004] - l21[[2]][,5:1004]
  p2120 <- l21[[2]][,5:1004] - l20[[2]][,5:1004]
  p2015 <- l20[[2]][,5:1004] - l15[[2]][,5:1004]
  p1510 <- l15[[2]][,5:1004] - l10[[2]][,5:1004]
  
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
  
  mat_table <- l10[[2]][,1:3] #%>% ungroup(con_id,pro_id,dis_id)
  mat5_diff <- cbind(mat_table,mat5_diff)
  write.csv(mat5_diff,"./shape/dis_diff.csv",col.names = T,row.names = F)
})


system.time({
  ####pro tiff####
  p2221 <- l22[[3]][,4:1003] - l21[[3]][,4:1003]
  p2120 <- l21[[3]][,4:1003] - l20[[3]][,4:1003]
  p2015 <- l20[[3]][,4:1003] - l15[[3]][,4:1003]
  p1510 <- l15[[3]][,4:1003] - l10[[3]][,4:1003]
  
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
  mat_table <- l10[[3]][,1:2] #%>% ungroup(con_id,pro_id,dis_id)
  mat5_diff <- cbind(mat_table,mat5_diff)
  write.csv(mat5_diff,"./shape/pro_diff.csv",col.names = T,row.names = F)
})

system.time({
  ####con tiff####
  p2221 <- l22[[4]][,3:1002] - l21[[4]][,3:1002]
  p2120 <- l21[[4]][,3:1002] - l20[[4]][,3:1002]
  p2015 <- l20[[4]][,3:1002] - l15[[4]][,3:1002]
  p1510 <- l15[[4]][,3:1002] - l10[[4]][,3:1002]
  
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
  
  mat_table <- l10[[4]][,1] #%>% ungroup(con_id,pro_id,dis_id)
  mat5_diff <- cbind(mat_table,mat5_diff)
  write.csv(mat5_diff,"./shape/con_diff.csv",col.names = T,row.names = F)
})








system.time({
  rm(list = ls())
  load("./Rdata/D3/pred_1000_2022.Rdata")
  l22 <- list_1000
  load("./Rdata/D3/pred_1000_2019.Rdata")
  l19 <- list_1000
  load("./Rdata/D3/pred_1000_2015.Rdata")
  l15 <- list_1000
  load("./Rdata/D3/pred_1000_2010.Rdata")
  l10 <- list_1000
  rm(list_1000)
  
  diff <- l10[[1]]-l22[[1]]
  r_diff <- (l10[[1]]-l22[[1]])/l10[[1]]
  
  mat5_diff <- matrix(NA,nrow = nrow(diff),ncol = 8)
  mat5_diff <- as.data.frame(mat5_diff)
  colnames(mat5_diff) <- c(
    paste0("diff",c("","_sd","_lo","_up"),sep=""),
    paste0("relative_diff",c("","_sd","_lo","_up"),sep="")
  )
  cl<-makeForkCluster(5)
  
  mat5_diff$diff <- parApply(cl,diff,MARGIN=1,mean)
  n = 1000
  mat5_diff$diff_sd = parApply(cl,diff,MARGIN=1,sd)
  mat5_diff$diff_lo <- parApply(cl,diff,MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5_diff$diff_up <-parApply(cl,diff,MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  rm(diff)
  gc()
  
  
  mat5_diff$relative_diff <- parApply(cl,r_diff,MARGIN=1,mean)
  mat5_diff$relative_diff_sd = parApply(cl,r_diff,MARGIN=1,sd)
  mat5_diff$relative_diff_lo <- parApply(cl,r_diff, MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5_diff$relative_diff_up <- parApply(cl,r_diff, MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  
  mat5_diff$p1 <- parApply(cl,l10[[1]],MARGIN=1,mean)
  stopCluster(cl)
  # mat5_diff$diff[which(mat5_diff$diff<0)] <- 0
  # mat5_diff$relative_diff[which(mat5_diff$diff<0)] <- 0
  # load("./Rdata/pred_csv0923.Rdata")
  load("~/Yu/project/Rdata/sorted_ids.Rdata")
  df.final <- data.frame(sorted_ids,
                         mat5_diff)
  
  geo_sf <- df.final %>% st_as_sf(coords = c(x = "LONNUM", y = "LATNUM"),crs=st_crs(4236)) %>% st_transform(32634)
  st_geometry(geo_sf) <-  "geometry"
  write_sf(geo_sf,"./shape/D3/5km_diff_22to10.shp")
})








system.time({
  rm(list = ls());gc()
  load("./Rdata/D1/pred_1000_2022.Rdata")
  l22 <- list_1000
  load("./Rdata/D1/pred_1000_2019.Rdata")
  l19 <- list_1000
  load("./Rdata/D1/pred_1000_2015.Rdata")
  l15 <- list_1000
  load("./Rdata/D1/pred_1000_2010.Rdata")
  l10 <- list_1000
  rm(list_1000)
  
  diff <- l10[[1]]-l22[[1]]
  r_diff <- (l10[[1]]-l22[[1]])/l10[[1]]
  
  mat5_diff <- matrix(NA,nrow = nrow(diff),ncol = 8)
  mat5_diff <- as.data.frame(mat5_diff)
  colnames(mat5_diff) <- c(
    paste0("diff",c("","_sd","_lo","_up"),sep=""),
    paste0("relative_diff",c("","_sd","_lo","_up"),sep="")
  )
  cl<-makeForkCluster(5)
  
  mat5_diff$diff <- parApply(cl,diff,MARGIN=1,mean)
  n = 1000
  mat5_diff$diff_sd = parApply(cl,diff,MARGIN=1,sd)
  mat5_diff$diff_lo <- parApply(cl,diff,MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5_diff$diff_up <-parApply(cl,diff,MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  rm(diff)
  gc()
  
  
  mat5_diff$relative_diff <- parApply(cl,r_diff,MARGIN=1,mean)
  mat5_diff$relative_diff_sd = parApply(cl,r_diff,MARGIN=1,sd)
  mat5_diff$relative_diff_lo <- parApply(cl,r_diff, MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5_diff$relative_diff_up <- parApply(cl,r_diff, MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  
  mat5_diff$p1 <- parApply(cl,l10[[1]],MARGIN=1,mean)
  stopCluster(cl)
  # mat5_diff$diff[which(mat5_diff$diff<0)] <- 0
  # mat5_diff$relative_diff[which(mat5_diff$diff<0)] <- 0
  # load("./Rdata/pred_csv0923.Rdata")
  load("~/Yu/project/Rdata/sorted_ids.Rdata")
  df.final <- data.frame(sorted_ids,
                         mat5_diff)
  
  geo_sf <- df.final %>% st_as_sf(coords = c(x = "LONNUM", y = "LATNUM"),crs=st_crs(4236)) %>% st_transform(32634)
  st_geometry(geo_sf) <-  "geometry"
  write_sf(geo_sf,"./shape/D1/5km_diff_22to10_d1.shp")
  
  
  
  
  
  
  
  
   diff <- l22[[4]][,3:1002]-l10[[4]][,3:1002]
  r_diff <- (diff)/l10[[4]][,3:1002]
  
  mat5_diff <- matrix(NA,nrow = nrow(diff),ncol = 8)
  mat5_diff <- as.data.frame(mat5_diff)
  colnames(mat5_diff) <- c(
    paste0("diff",c("","_sd","_lo","_up"),sep=""),
    paste0("relative_diff",c("","_sd","_lo","_up"),sep="")
  )
  cl<-makeForkCluster(5)
  
  mat5_diff$diff <- parApply(cl,diff,MARGIN=1,mean)
  n = 1000
  mat5_diff$diff_sd = parApply(cl,diff,MARGIN=1,sd)
  mat5_diff$diff_lo <- parApply(cl,diff,MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5_diff$diff_up <-parApply(cl,diff,MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  rm(diff)
  gc()
  
  
  mat5_diff$relative_diff <- parApply(cl,r_diff,MARGIN=1,mean)
  mat5_diff$relative_diff_sd = parApply(cl,r_diff,MARGIN=1,sd)
  mat5_diff$relative_diff_lo <- parApply(cl,r_diff, MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5_diff$relative_diff_up <- parApply(cl,r_diff, MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  
  mat5_diff$p1 <- parApply(cl,l10[[4]][,3:1002],MARGIN=1,mean)
  stopCluster(cl)
  mat_table <- l10[[4]][,1] #%>% ungroup(con_id,pro_id,dis_id)
  mat5_diff <- cbind(mat_table,diff)
  write.csv(mat5_diff,"./shape/con_diff_20to10.csv",col.names = T,row.names = F)
  
  
  
  rm(list = ls())
  load("./Rdata/D1/pred_1000_2022.Rdata")
  l22 <- list_1000
  load("./Rdata/D1/pred_1000_2019.Rdata")
  l19 <- list_1000
  load("./Rdata/D1/pred_1000_2015.Rdata")
  l15 <- list_1000
  load("./Rdata/D1/pred_1000_2010.Rdata")
  l10 <- list_1000
  rm(list_1000)
  
  diff <- l22[[4]][,3:1002]-l10[[4]][,3:1002]
  r_diff <- (diff)/l10[[4]][,3:1002]
  
  mat5_diff <- matrix(NA,nrow = nrow(diff),ncol = 8)
  mat5_diff <- as.data.frame(mat5_diff)
  colnames(mat5_diff) <- c(
    paste0("diff",c("","_sd","_lo","_up"),sep=""),
    paste0("relative_diff",c("","_sd","_lo","_up"),sep="")
  )
  cl<-makeForkCluster(5)
  
  mat5_diff$diff <- parApply(cl,diff,MARGIN=1,mean)
  n = 1000
  mat5_diff$diff_sd = parApply(cl,diff,MARGIN=1,sd)
  mat5_diff$diff_lo <- parApply(cl,diff,MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5_diff$diff_up <-parApply(cl,diff,MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  rm(diff)
  gc()
  
  
  mat5_diff$relative_diff <- parApply(cl,r_diff,MARGIN=1,mean)
  mat5_diff$relative_diff_sd = parApply(cl,r_diff,MARGIN=1,sd)
  mat5_diff$relative_diff_lo <- parApply(cl,r_diff, MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5_diff$relative_diff_up <- parApply(cl,r_diff, MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  
  mat5_diff$p1 <- parApply(cl,l10[[4]][,3:1002],MARGIN=1,mean)
  stopCluster(cl)
  mat_table <- l10[[4]][,1] #%>% ungroup(con_id,pro_id,dis_id)
  mat5_diff <- cbind(mat_table,diff)
  write.csv(mat5_diff,"./shape/con_diff_20to10_d1.csv",col.names = T,row.names = F)
})
