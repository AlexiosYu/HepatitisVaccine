rm(list = ls());gc()
library(sf);library(doParallel);library(magrittr)

load("./Rdata/pred_csv0923.Rdata")
for (i in c(10:22)) {
  load(sprintf("./Rdata/D3/pred_1000_20%d.Rdata",i))
  l31 <- list_1000
  load(sprintf("./Rdata/D1/pred_1000_20%d.Rdata",i))
  l11 <- list_1000

  load("./Rdata/pred_csv0923.Rdata")
  load("~/Yu/project/Rdata/sorted_ids.Rdata")

  
  ########all Africa#######
  p3 <- l31[[1]]
  p1 <- l11[[1]]

  yearint <- 2000+i
  pop <- pred_csv$pop[which(pred_csv$B==yearint)]
  p1 <- pop*p1
  p3 <- pop*p3
  mat5 <- matrix(NA,nrow = 1,ncol = 8)
  mat5 <- as.data.frame(mat5)
  colnames(mat5) <- c(
    paste0("p1",c("","_sd","_lo","_up"),sep=""),
    paste0("p3",c("","_sd","_lo","_up"),sep="")
  )
  p1 <- colSums(p1)/sum(pop)
  p3 <- colSums(p3)/sum(pop)
  mat5$p1 <- mean(p1)
  mat5$p1_sd <- sd(p1)
  mat5$p1_lo <- quantile(p1,0.025)
  mat5$p1_up <- quantile(p1,0.975)
  mat5$p3 <- mean(p3)
  mat5$p3_sd <- sd(p3)
  mat5$p3_lo <- quantile(p3,0.025)
  mat5$p3_up <- quantile(p3,0.975)
  write.csv(mat5,sprintf("./shape/all_p3p1_20%d.csv",i),col.names = T,row.names = F)
  ########con id#######
  p3 <- l31[[4]][,3:1002]
  p1 <- l11[[4]][,3:1002]
  
  
  gc()
  mat5 <- matrix(NA,nrow = nrow(p1),ncol = 8)
  mat5 <- as.data.frame(mat5)
  colnames(mat5) <- c(
    paste0("p1",c("","_sd","_lo","_up"),sep=""),
    paste0("p3",c("","_sd","_lo","_up"),sep="")
  )
  cl<-makeForkCluster(5)
  mat5$p1 <- parApply(cl,p1,MARGIN=1,mean)
  n = 1000
  mat5$p1_sd = parApply(cl,p1,MARGIN=1,sd)
  mat5$p1_lo <- parApply(cl,p1,MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5$p1_up <-parApply(cl,p1,MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  rm(p1)
  gc()
  
  
  mat5$p3 <- parApply(cl,p3,MARGIN=1,mean)
  mat5$p3_sd = parApply(cl,p3,MARGIN=1,sd)
  mat5$p3_lo <- parApply(cl,p3, MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5$p3_up <- parApply(cl,p3, MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  
  rm(p3,p3,p1)
  stopCluster(cl)
  
  mat_table <- l11[[4]][,1] #%>% ungroup(con_id,pro_id)
  mat5 <- cbind(mat_table,mat5)
  write.csv(mat5,sprintf("./shape/con_p3p1_20%d.csv",i),col.names = T,row.names = F)
  ########pro id#######
  p3 <- l31[[3]][,4:1003]
  p1 <- l11[[3]][,4:1003]
  
  
  gc()
  mat5 <- matrix(NA,nrow = nrow(p1),ncol = 8)
  mat5 <- as.data.frame(mat5)
  colnames(mat5) <- c(
    paste0("p1",c("","_sd","_lo","_up"),sep=""),
    paste0("p3",c("","_sd","_lo","_up"),sep="")
  )
  cl<-makeForkCluster(5)
  mat5$p1 <- parApply(cl,p1,MARGIN=1,mean)
  n = 1000
  mat5$p1_sd = parApply(cl,p1,MARGIN=1,sd)
  mat5$p1_lo <- parApply(cl,p1,MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5$p1_up <-parApply(cl,p1,MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  rm(p1)
  gc()
  
  
  mat5$p3 <- parApply(cl,p3,MARGIN=1,mean)
  mat5$p3_sd = parApply(cl,p3,MARGIN=1,sd)
  mat5$p3_lo <- parApply(cl,p3, MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5$p3_up <- parApply(cl,p3, MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  
  
  rm(p3,p3,p1)
  stopCluster(cl)
  
  mat_table <- l11[[3]][,1:2] #%>% ungroup(con_id,pro_id)
  mat5 <- cbind(mat_table,mat5)
  write.csv(mat5,sprintf("./shape/pro_p3p1_20%d.csv",i),col.names = T,row.names = F)
  
  ########dis id#######
  p3 <- l31[[2]][,5:1004]
  p1 <- l11[[2]][,5:1004]
  gc()
  mat5 <- matrix(NA,nrow = nrow(p1),ncol = 8)
  mat5 <- as.data.frame(mat5)
  colnames(mat5) <- c(
    paste0("p1",c("","_sd","_lo","_up"),sep=""),
    paste0("p3",c("","_sd","_lo","_up"),sep="")
  )
  cl<-makeForkCluster(5)
  mat5$p1 <- parApply(cl,p1,MARGIN=1,mean)
  n = 1000
  mat5$p1_sd = parApply(cl,p1,MARGIN=1,sd)
  mat5$p1_lo <- parApply(cl,p1,MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5$p1_up <-parApply(cl,p1,MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  rm(p1)
  gc()
  
  
  mat5$p3 <- parApply(cl,p3,MARGIN=1,mean)
  mat5$p3_sd = parApply(cl,p3,MARGIN=1,sd)
  mat5$p3_lo <- parApply(cl,p3, MARGIN=1, function(x) quantile(x,0.025,na.rm=T))
  mat5$p3_up <- parApply(cl,p3, MARGIN=1, function(x) quantile(x,0.975,na.rm=T))
  
  rm(p3,p3,p1)
  stopCluster(cl)
  
  mat_table <- l11[[2]][,1:3] #%>% ungroup(con_id,pro_id,dis_id)
  mat5 <- cbind(mat_table,mat5)
  write.csv(mat5,sprintf("./shape/dis_relat_p3p1_20%d.csv",i),col.names = T,row.names = F)
}
