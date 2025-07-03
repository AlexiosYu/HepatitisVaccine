rm(list=ls())
library(sf);library(magrittr)
setwd("~/Yu/project/")
load("./data/dataall_0515.Rdata")
africa_shp <- sf::read_sf("/d2/home/guest3/Malaria/whz/Africa_0.shp") %>% 
  st_make_valid()
#csv_list <- list.files(pattern = ".csv",path = "./covar/",full.names = T)

source("./code/RegularizeProcessCV.R")

processed <- data_process_function(data=data_all,shp_sf=africa_shp,n = "HEP1_1",N = "HEP_E")
processed$processed_data %>% nrow()

source("./code/makePolygon.R")
poly_res <- ploy_process_function(processed)

source("./code/ml_function.R")
ml_res <- sub_model_function(processed, nfolds = 5,methods =c("glmnet", "gam",  "ranger"))
gc();
cor_resdata <- ml_res$correlations
model_names <- c("las","gam","rf")


source("./code/stack_synthesizer_cv.R")
source("./code/output_calc.R")
source("./code/cv_data_extratcor.R")



gp.output <- list()
gp.stack <- list()
for (i in 1:5) {
  stack_fusion <- stack_synthesizer_function(poly_res$mesh, ml_res,processed,model_names,fold_i = i)
  theta <- c(-2.3641, 1.7302642, 0.2704400 ,3.1916643 ,-1.2972458 ,-1.3005228, 0.6161641)
  output <- output_calc_function(stack_fusion,theta)
  cat(sprintf("the Run %d is over",i))
  index_est_cv <- inla.stack.index(stack_fusion$stack,"est")$data
  p_marginals_cv <- output$output$marginals.fitted.values[index_est_cv]
  cll <- makeForkCluster(5)
  p_mean <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = mean_f)
  p_mean_s <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = sd_f)
  p_mean_lo <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = lo_f)
  p_mean_up <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = up_f)
  stopCluster(cll)
  gp.output[[i]] <- data.frame(mean=p_mean, sd=p_mean_s, lo=p_mean_lo, up=p_mean_up)
  gp.stack[[i]] <- stack_fusion
  save(list = c("gp.output","gp.stack"),file = sprintf("./Rdata/gp_1_%s.Rdata",i))
  gc()
}

df.cv <- data.frame()
for(i in 1:5){
  inx <- gp.stack[[i]]$inx
  mean <- gp.output[[i]]$mean[inx]
  sd <- gp.output[[i]]$sd[inx]
  lo <- gp.output[[i]]$lo[inx]
  up <- gp.output[[i]]$up[inx]
  obs <- gp.stack[[i]]$obs_i
  res <- data.frame(
    obs=gtools::inv.logit(obs),mean=mean,lo=lo,up=up
  )
  df.cv <- rbind(df.cv,res)
}


df.cv.name <- paste("./data/df.cv_d1_0531.csv", sep = "")
write.csv(df.cv, file=df.cv.name, row.names = F)
cor(df.cv$obs,df.cv$mean)
model <- lm(obs ~ mean, data = df.cv)
# 获取模型摘要
model_summary <- summary(model)
model_summary$r.squared
