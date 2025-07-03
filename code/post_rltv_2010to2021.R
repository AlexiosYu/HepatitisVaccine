rm(list = ls());gc()
library(sf);library(doParallel);library(magrittr)

load("./Rdata/pred_csv0923.Rdata")
for (i in c(10:22)) {
  load(sprintf("./Rdata/D3/pred_1000_20%d.Rdata",i))
  l31 <- list_1000
  load(sprintf("./Rdata/D1/pred_1000_20%d.Rdata",i))
  l11 <- list_1000
  p31 <- l31[[1]]
  p11 <- l11[[1]]
  diff <- p11-p31
  r_diff <- (p11-p31)/p11
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
  
  mat5_diff$p1 <- parApply(cl,p11,MARGIN=1,mean)
  
  rm(r_diff,p31,p11)
  stopCluster(cl)
  
  mat5_diff$diff[which(mat5_diff$diff<0)] <- 0
  mat5_diff$relative_diff[which(mat5_diff$diff<0)] <- 0
  load("./Rdata/pred_csv0923.Rdata")
  load("~/Yu/project/Rdata/sorted_ids.Rdata")
  df.final <- data.frame(sorted_ids,
                         mat5_diff)
  
  geo_sf <- df.final %>% st_as_sf(coords = c(x = "LONNUM", y = "LATNUM"),crs=st_crs(4236)) %>% st_transform(32634)
  st_geometry(geo_sf) <-  "geometry"
  write_sf(geo_sf,sprintf("./shape/5km_relative_diff_20%d.shp",i))
  
  ########all Africa#######
  p31 <- l31[[1]]
  p11 <- l11[[1]]
  diff <- p11-p31
  r_diff <- (p11-p31)/p11
  yearint <- 2000+i
  pop <- pred_csv$pop[which(pred_csv$B==yearint)]
  diff <- pop*diff
  r_diff <- pop*r_diff
  mat5_diff <- matrix(NA,nrow = 1,ncol = 8)
  mat5_diff <- as.data.frame(mat5_diff)
  colnames(mat5_diff) <- c(
    paste0("diff",c("","_sd","_lo","_up"),sep=""),
    paste0("relative_diff",c("","_sd","_lo","_up"),sep="")
  )
  diff <- colSums(diff)/sum(pop)
  r_diff <- colSums(r_diff)/sum(pop)
  mat5_diff$diff <- mean(diff)
  mat5_diff$diff_sd <- sd(diff)
  mat5_diff$diff_lo <- quantile(diff,0.025)
  mat5_diff$diff_up <- quantile(diff,0.975)
  mat5_diff$relative_diff <- mean(r_diff)
  mat5_diff$relative_diff_sd <- sd(r_diff)
  mat5_diff$relative_diff_lo <- quantile(r_diff,0.025)
  mat5_diff$relative_diff_up <- quantile(r_diff,0.975)
  write.csv(mat5_diff,sprintf("./shape/all_diff_20%d.csv",i),col.names = T,row.names = F)
  ########pro id#######
  p31 <- l31[[3]][,4:1003]
  p11 <- l11[[3]][,4:1003]
  diff <- p11-p31
  r_diff <- (p11-p31)/p11
  gc()
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
  
  mat5_diff$p1 <- parApply(cl,p11,MARGIN=1,mean)
  
  rm(r_diff,p31,p11)
  stopCluster(cl)
  
  mat5_diff$diff[which(mat5_diff$diff<0)] <- 0
  mat5_diff$relative_diff[which(mat5_diff$diff<0)] <- 0
  mat_table <- l11[[3]][,1:2] #%>% ungroup(con_id,pro_id)
  mat5_diff <- cbind(mat_table,mat5_diff)
  write.csv(mat5_diff,sprintf("./shape/pro_relat_diff_20%d.csv",i),col.names = T,row.names = F)
  
  ########dis id#######
  p31 <- l31[[2]][,5:1004]
  p11 <- l11[[2]][,5:1004]
  diff <- p11-p31
  r_diff <- (p11-p31)/p11
  gc()
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
  
  mat5_diff$p1 <- parApply(cl,p11,MARGIN=1,mean)
  
  rm(r_diff,p31,p11)
  stopCluster(cl)
  
  mat5_diff$diff[which(mat5_diff$diff<0)] <- 0
  mat5_diff$relative_diff[which(mat5_diff$diff<0)] <- 0
  mat_table <- l11[[2]][,1:3] #%>% ungroup(con_id,pro_id,dis_id)
  mat5_diff <- cbind(mat_table,mat5_diff)
  write.csv(mat5_diff,sprintf("./shape/dis_relat_diff_20%d.csv",i),col.names = T,row.names = F)
  p31 <- l31[[4]][,3:1002]
  p11 <- l11[[4]][,3:1002]
  diff <- p11-p31
  r_diff <- (p11-p31)/p11
  gc()
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
  
  mat5_diff$p1 <- parApply(cl,p11,MARGIN=1,mean)
  
  rm(r_diff,p31,p11)
  stopCluster(cl)
  
  mat5_diff$diff[which(mat5_diff$diff<0)] <- 0
  mat5_diff$relative_diff[which(mat5_diff$diff<0)] <- 0
  mat_table <- l11[[4]][,1] #%>% ungroup(con_id,pro_id,dis_id)
  mat5_diff <- cbind(mat_table,mat5_diff)
  write.csv(mat5_diff,sprintf("./shape/con_relat_diff_20%d.csv",i),col.names = T,row.names = F)
}


#计算rdiff的改变
load(sprintf("./Rdata/D3/pred_1000_20%d.Rdata",10))
l31 <- list_1000
load(sprintf("./Rdata/D1/pred_1000_20%d.Rdata",10))
l11 <- list_1000
p31 <- l31[[2]][,5:1004]
p11 <- l11[[2]][,5:1004]
r_diff_10 <- (p11-p31)/p11

load(sprintf("./Rdata/D3/pred_1000_20%d.Rdata",22))
l31 <- list_1000
load(sprintf("./Rdata/D1/pred_1000_20%d.Rdata",22))
l11 <- list_1000
p31 <- l31[[2]][,5:1004]
p11 <- l11[[2]][,5:1004]
r_diff_22 <- (p11-p31)/p11
r_diffr <- r_diff_10 - r_diff_22
mat5_diff <- matrix(NA,nrow = nrow(r_diffr),ncol = 4)
mat5_diff <- as.data.frame(mat5_diff)
colnames(mat5_diff) <- c(
  paste0("relative_diff",c("","_sd","_lo","_up"),sep="")
)
cl<-makeForkCluster(5)

n = 1000
mat5_diff$relative_diff <- parApply(cl,r_diffr,MARGIN=1,mean)
mat5_diff$relative_diff_sd = parApply(cl,r_diffr,MARGIN=1,sd)
mat5_diff$relative_diff_lo <- parApply(cl,r_diffr, MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
mat5_diff$relative_diff_up <- parApply(cl,r_diffr, MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
stopCluster(cl)
mat5_diff <- mat5_diff %>% 
  mutate(sign = relative_diff_sd*relative_diff_up)
t <- mat5_diff[which(mat5_diff$sign>0&mat5_diff$relative_diff_up<0),]
dpp <- apply(r_diffr, 2, function(x){
  length(which(x>0))/length(x)
})
mean(dpp)
quantile(dpp,c(0.025,0.975))
