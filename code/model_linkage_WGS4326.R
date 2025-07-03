suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
model_linkage <- function(idx_year,pred_res,model_names){
  nlist <- which(pred_res$pred_csv$time == idx_year)
  lpv_all <- pred_res$local.pred.cov
  Intercept <- pred_res$Intercept_vec
  pred_year <- pred_res$pred_csv %>% 
    filter(time == idx_year)
  
  
  cat("Start processing,step1 linear predictor term...\n\n")
  #------------step1-------------------
  ###calculate the linear result
  new_pred_cov <- lapply(1:3, function(x){
    return(lpv_all[[x]][nlist])
  }) %>% do.call("cbind",.)
  #new_pred_cov <- new_pred_cov[nlist,]
  col_names <- colnames(pred_res$draws_hyp)
  selected_cols <- sapply(model_names, function(model) {
    grep(paste0( model), col_names)
  })
  ml_beta <- pred_res$draws_hyp[, selected_cols] %>% 
    t()
  linear_predictor <- new_pred_cov %*% ml_beta %>% 
    sweep(., 2, Intercept, FUN = "+")
  
  cat("Start processing,step2 spatio temporal term...\n\n")
  #------------step2-------------------
  ###calculate the spatio-temporal result
  cat(sprintf("the dim of linear_predictor is: \n%d \t %d\n",dim(linear_predictor)[1],dim(linear_predictor)[2]))
  A.pred <- pred_res$A[nlist,]
  
  pred_s <- pred_res$pred_s
  cat(sprintf("A.pred %s %s",dim(A.pred)[1],dim(A.pred)[2]))
  cat(sprintf("pred_s %s %s",dim(pred_s)[1],dim(pred_s)[2]))
  field <- A.pred %*% pred_res$pred_s
  field <- as.matrix(field)
  
  #------------step3-------------------
  ###calculate the inla result of two step logistic bayesian model
  pred <- inla.link.invlogit(linear_predictor + field)
  load("./Rdata//sorted_ids.Rdata")
  
  table_property_column_temp <- pred_res$pred_csv[nlist,] %>% 
    dplyr::select(ID,LONNUM,LATNUM,B,population)
  
  table_property_column <- left_join(table_property_column_temp,sorted_ids)%>% 
    dplyr::select(ID,LONNUM,LATNUM,con_id,pro_id,dis_id,B,population)
  cat("left join with our ids data")
  pred_population <-  sapply(1:1000,function(x) {
    pred[,x]*table_property_column$population
  })
  return(list(pred = pred,
              table_property_column = table_property_column,
              pred_population = pred_population))
}
