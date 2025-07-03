rake_poly_function_halfway <- function(ncluster, adjusted_pred,adjusted_tibs, grouping_vars,selected_year_index) {
  # Extracting the necessary data
  title_pred <- adjusted_tibs
  pred <- adjusted_pred
  grouping_vars <- c(grouping_vars)
  time0 <- Sys.time()
  cat(paste(time0,"initializing"))
  cat("\n")
  # Making fork cluster for parallel calculation
  cl <- parallel::makeForkCluster(ncluster)
  time_1 <- Sys.time()
  cat(paste(time_1,"CLUSTER"))
  cat("\n")
  grouping_vars <- c(grouping_vars,"B")
  # group_by function to calculate the probability 
  result_matr <- parLapplyLB (cl, 1:1000, function(i){
    i_pred <- pred[,i] %>% as.data.frame()
    colnames(i_pred) <- "a"
    matr <- cbind(title_pred,i_pred) %>% 
      group_by(!!!rlang::syms(grouping_vars)) %>% 
      dplyr::summarise(p = sum(a*population,na.rm = T)/sum(population,na.rm = T)) %>% 
      ungroup(!!!rlang::syms(grouping_vars))
    colnames(matr)[ncol(matr)] <- sprintf("p_%d",i)
    return(matr)
  })  %>% 
    reduce(.,left_join,by = grouping_vars)
  parallel::stopCluster(cl)
  # 分离含有 "p_" 的列和其他列
  cols_with_p <- result_matr[, grepl("^p_", colnames(result_matr))]
  col_without_p <- result_matr[, !grepl("^p_", colnames(result_matr))]
  

  
  return(list(
    pred=cols_with_p,
    tib=col_without_p
  ))
}
