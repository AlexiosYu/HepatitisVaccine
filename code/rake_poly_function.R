rake_poly_function <- function(ncluster, adjusted_pred,adjusted_tibs, grouping_vars,selected_year_index) {
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
  
  # 分离含有 "p_" 的列和其他列
  cols_with_p <- result_matr[, grepl("^p_", colnames(result_matr))]
  col_without_p <- result_matr[, !grepl("^p_", colnames(result_matr))]
  
  # 计算统计量
  xbar <- parallel::parApply(cl, cols_with_p, MARGIN = 1, FUN = mean, na.rm = TRUE)
  time1 <- Sys.time()
  cat(paste(time1,"x"))
  cat("\n")
  s <- parallel::parApply(cl, cols_with_p, MARGIN = 1, FUN = sd, na.rm = TRUE)
  time2 <- Sys.time()
  cat(paste(time2,"\t"))
  cat("\n")
  qt <- parallel::parApply(cl, cols_with_p, MARGIN = 1, FUN = function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE)) %>%
    t() %>%
    as.data.frame()
  time3 <- Sys.time()
  cat(time3)
  cat("\n")
  # 关闭集群
  parallel::stopCluster(cl)
  
  # 构建最终数据框
  df_final <- data.frame(
    col_without_p,
    mean = xbar,
    sd = s,
    low = qt$`2.5%`,
    up = qt$`97.5%`
  )
  
  return(df_final)
}
