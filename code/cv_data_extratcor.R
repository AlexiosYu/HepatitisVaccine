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



cv_data_extratcor <- function(stack_funsion,output){
  index_est_cv <- inla.stack.index(stack_funsion$stack,"est")$data
  p_marginals_cv <- output$output$marginals.fitted.values[index_est_cv]
  
  cll <- makeForkCluster(2)
  p_mean <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = mean_f)
  p_mean_s <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = sd_f)
  p_sd <- sqrt(p_mean_s - p_mean^2 )
  p_mean_lo <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = lo_f)
  p_mean_up <- parSapplyLB(cll,1:length(p_marginals_cv),FUN = up_f)
  stopCluster(cll)
  
  spl.cv <- data.frame(mean=p_mean, sd=p_sd, lo=p_mean_lo, up=p_mean_up)
  
  return(spl.cv)
}