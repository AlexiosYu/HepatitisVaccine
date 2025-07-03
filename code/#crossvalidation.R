suppressMessages(
  system.time({
    require(gstat);require(geoR);
    require(fields);require(sf);
    require(lattice);require(spdep);
    require(boot);require(plyr);
    require(tidyr);require(reshape2);
    require(ggplot2);
    require(maptools);require(rgdal);
    require(raster);require(xlsx);
    require(sp);require(INLA)
    require(raster);require(remotes)
    require(readxl);require(terra)
    require(inlabru);require(maps)
    require(MASS);require(MBA)
    require(combinat);require(gtools)
    require(matrixStats);require(rgeos)
    require(caret);library(doParallel)
  })
)
rm(list=ls())
load(sprintf("./Rdata/model_%s.Rdata","HEP3"))
ml_res <- res$ml_res



mean_f <- function(x){
  inla.emarginal(inla.link.invlogit, p_marginals_cv[[x]])
}
lo_f <- function(x){
  inla.qmarginal(0.025, inla.tmarginal(inla.link.invlogit, p_marginals_cv[[x]]))}
sd_f <- function(x){
  inla.emarginal(function(x) (exp(x)/(1+exp(x)))^2, p_marginals_cv[[x]])
}
up_f <- function(x){
  inla.qmarginal(0.975, inla.tmarginal(inla.link.invlogit, p_marginals_cv[[x]]))}


gp.output <- list()
mod.pred.cv.st <- list()
for(i in 1:5){
  tmpY<-res$ml_res$
  tmpY[data_all$nfolds==i]=NA
  group <- data_all$time - min(data_all$time) + 1
  A.est <-  inla.spde.make.A(mesh = res$poly_res$mesh,
                             loc = as.matrix(data.frame(data_all[,c("x","y")])),
                             group=group)
  dim(A.est) 
  cov_i <- res$ml_res$cv_predictions %>% 
    lapply(., function(x){
      x[["predictions"]]
    })
  #cov_i$las[which(data_all$folds==i)] <- local.obs.cov$las[which(data_all$folds==i)]
  #cov_i$gam[which(data_all$folds==i)] <- local.obs.cov$gam[which(data_all$folds==i)]
  #cov_i$rf[which(data_all$folds==i)] <- local.obs.cov$rf[which(data_all$folds==i)]
  stack.est.cv =
    inla.stack(tag="est",
               data = list(Y.elect=tmpY,
                           wts=data_all$weight),
               A=list(A.est,1),
               effects=list(c(res$stack_funsion$s.index,list(Intercept=1)),
                            c(cov_i)
               )
    )
  
  inla.setOption("enable.inla.argument.weights", TRUE)
  inla.setOption(num.threads=30)
  formula.inla <- as.formula(paste(
    paste("Y.elect ~ -1 + Intercept + "),
    paste("f(st.field, model=spde,group=st.field.group, control.group=list(model='ar1'))+",sep=""),	
    paste("f(glmnet, model='clinear',range=c(0,1),initial=-0.2) +",sep=""),	
    paste("f(gam, model='clinear',range=c(0,1),initial=-0.2) +",sep=""),
    paste("f(ranger, model='clinear',range=c(0,1),initial=0.2) ",sep=""),
    sep=""))
  mod.pred.cv.st[[i]] =   inla(formula.inla,
                               data=inla.stack.data(stack.est.cv, spde=res$stack_funsion$spde),
                               family="gaussian",
                               control.predictor=list(A=inla.stack.A(stack.est.cv), compute=F,link=1),
                               weights = stack.est.cv$data$data$wts,
                               control.inla= list(strategy = 'gaussian',
                                                  int.strategy='eb',
                                                  fast=TRUE,dz=1,
                                                  step.factor=1,
                                                  stupid.search=FALSE),
                               control.mode = list(theta=c(-1.9610075,2.3722650,-0.5334459,2.1654812,-2.3582584,-2.3850706,2.6193532)),
                               verbose=TRUE
  )      
  
  index_est_cv <- inla.stack.index(stack.est.cv,"est")$data
  p_marginals_cv <- mod.pred.cv.st[[i]]$marginals.fitted.values[index_est_cv]
  
  cll <- makeForkCluster(10)
  p_mean <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = mean_f)
  p_mean_s <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = sd_f)
  p_sd <- sqrt(p_mean_s - p_mean^2 )
  p_mean_lo <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = lo_f)
  p_mean_up <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = up_f)
  stopCluster(cll)
  
  spl.cv <- data.frame(mean=p_mean, sd=p_sd, lo=p_mean_lo, up=p_mean_up)
  gp.output[[i]] <- spl.cv
  save(gp.output,file=paste("./cv_folder/gp.output.with","_",i,".Rdata",sep=""))
}

cv.gp.st<- data.frame(mean=rep(NA,length(data_all$Y.elect)), sd=rep(NA,length(data_all$Y.elect)), lo=rep(NA,length(data_all$Y.elect)), up=rep(NA,length(data_all$Y.elect)))
for(i in 1:5){
  cv.gp.st$mean[data_all$nfolds==i]=gp.output[[i]]$mean[data_all$nfolds==i]
  cv.gp.st$sd[data_all$nfolds==i]=gp.output[[i]]$sd[data_all$nfolds==i]
  cv.gp.st$lo[data_all$nfolds==i]=gp.output[[i]]$lo[data_all$nfolds==i]
  cv.gp.st$up[data_all$nfolds==i]=gp.output[[i]]$up[data_all$nfolds==i]
}

df.cv <- data.frame(obs = inv.logit(data_all$Y.elect), cv.gp.st)
df.cv.name <- paste("./data/df.cv_1017.csv", sep = "")
write.csv(df.cv, file=df.cv.name, row.names = F)
