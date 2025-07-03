load("diff_all.Rdata")

system.time({
  ####5km tiff####
  p2210 <- al_list[[5]] - al_list[[1]]
  
  mat5_diff <- matrix(NA,nrow = nrow(p2210),ncol = 1) %>% as.data.frame()
  colnames(mat5_diff) <- c(
    paste0("diff_",5,sep="")
  )
  cl<-makeForkCluster(5)
  
  mat5_diff$diff_5 <- parApply(cl,p2210,MARGIN=1,mean)

  stopCluster(cl)
  gc()
  
  df.final <- data.frame(tib_list[1],
                         mat5_diff)
  
  geo_sf <- df.final %>% st_as_sf(coords = c(x = "X", y = "Y"),crs=st_crs(32634))
  st_geometry(geo_sf) <-  "geometry"
  write_sf(geo_sf,"./shape/5km_diff_rake_2210.shp")
  rm(geo_sf,mat5_diff)
})

####dis tiff####
p2210 <- con_all[[5]]$pred-con_all[[1]]$pred
mat5_diff <- matrix(NA,nrow = nrow(p2210),ncol = 1) %>% as.data.frame()
colnames(mat5_diff) <- c(
  paste0("diff_",5,sep="")
)
View(pred_con)
View(dis_all)
cl<-makeForkCluster(5)
mat5_diff$diff_5 <- parApply(cl,p2210,MARGIN=1,mean)
stopCluster(cl)
stopCluster(cl)
gc()
mat5_diff <- cbind(con_all[[1]]$tib,mat5_diff)
write.csv(mat5_diff,"./shape/con_diff_rake_2210.csv",col.names = T,row.names = F)
system.time({
  ####dis tiff####
  p2210 <- pro_all[[5]]$pred-pro_all[[1]]$pred
  mat5_diff <- matrix(NA,nrow = nrow(p2210),ncol = 1) %>% as.data.frame()
  colnames(mat5_diff) <- c(
    paste0("diff_",5,sep="")
  )
  cl<-makeForkCluster(5)
  mat5_diff$diff_5 <- parApply(cl,p2210,MARGIN=1,mean)
  stopCluster(cl)
  gc()
  mat5_diff <- cbind(pro_all[[1]]$tib,mat5_diff)
  write.csv(mat5_diff,"./shape/pro_diff_rake_2210.csv",col.names = T,row.names = F)
})
system.time({
  ####dis tiff####
  p2210 <- dis_all[[5]]$pred-dis_all[[1]]$pred
  mat5_diff <- matrix(NA,nrow = nrow(p2210),ncol = 1) %>% as.data.frame()
  colnames(mat5_diff) <- c(
    paste0("diff_",5,sep="")
  )
  cl<-makeForkCluster(5)
  mat5_diff$diff_5 <- parApply(cl,p2210,MARGIN=1,mean)
  stopCluster(cl)
  gc()
  mat5_diff <- cbind(dis_all[[1]]$tib,mat5_diff)
  write.csv(mat5_diff,"./shape/dis_diff_rake_2210.csv",col.names = T,row.names = F)
})
rm(list=ls())
gc()