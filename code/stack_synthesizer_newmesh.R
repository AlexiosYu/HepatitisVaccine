suppressMessages(require(dplyr))
suppressMessages(require(INLA))
create_pc_matern_spde <- function(mesh, 
                                  alpha = 2,
                                  prior.pc.rho,
                                  prior.pc.sig) {
  manifold_type <- mesh$manifold
  d <- switch(manifold_type,
              R2 = 2, 
              S2 = 2,
              R1 = 1,
              stop("unsupported anifold_types of mesh:", manifold_type))
  
  # 计算Matern参数
  nu <- alpha - d/2  # 平滑度参数
  if(nu <= 0) stop("alpha参数过小，需要满足alpha > d/2")
  
  # 构建PC先验参数 (参考INLA文档)
  pc_prior <- list(
    range = prior.pc.rho,
    sigma = prior.pc.sig
  )
  
  # 创建SPDE模型 (使用新版PC prior接口)
  spde <- INLA::inla.spde2.pcmatern(
    mesh = mesh,
    alpha = alpha,  # Matern平滑度
    prior.range = pc_prior$range, # P(range < range0) = prob
    prior.sigma = pc_prior$sigma, # P(sigma > sigma0) = prob
    constr = TRUE   # 施加可积约束
  )
  
  # 调试信息
  if (getOption("verbose.spde", FALSE)) {
    message("SPDE:")
    message(sprintf("dim: %dD", d))
    message(sprintf("Matern para: nu=%.2f", nu))
    message(sprintf("PC prior: range=(%g, %g), sigma=(%g, %g)",
                    pc_prior$range[1], pc_prior$range[2],
                    pc_prior$sigma[1], pc_prior$sigma[2]))
  }
  
  return(spde)
}

stack_synthesizer_function <- function(mesh, ml_res,processed,model_names) {
  time_a <- Sys.time()
  cat("Data checking in progress...\n\n")
  class_1 <- class(mesh)
  class_2 <- class(ml_res)
  class_3 <- class(processed)
  class_4 <- class(model_names)
  
  if(("inla.mesh" == class_1)&&("list" == class_2)&&("list" == class_3)&&("character" == class_4)){
    cat("The classifications of input data are valid, going on...\n\n")
  }else{
    stop("INvalid input,please re-check!n\n")
  }
  
  cat("Start processing,step1...\n\n")
  #------------step1-------------------
  ###extract data and stack them all
  model_pred <- lapply(names(ml_res$cv_predictions), function(x){
    dat <- ml_res$cv_predictions[[x]] %>% 
      dplyr::rename(!!x:="predictions")
  }) %>% Reduce("left_join",.)
  grouped_data <- left_join(ml_res$data_all,model_pred)%>% 
    mutate(group = time - min(time) + 1 )
  Y.elect <-  grouped_data$Y.elect
  ##select submodel###
  model_names <- unique(model_names)
  selected_submodels <- lapply(model_names, function(x) {
    switch(x,
           "las" = grouped_data$glmnet,
           "gam"   = grouped_data$gam,
           "svm"   = grouped_data$svmRadial,
           "rf"    = grouped_data$ranger,
           NULL)
  })
  
  local.est.cov <- setNames(selected_submodels , model_names)
  loc.spde <- create_pc_matern_spde(
    mesh = mesh,
    prior.pc.rho = c(0.3, 0.5), # P(range < rho0) = 0.5
    prior.pc.sig = c(0.5, 0.5), # P(sigma > sig0) = 0.5
    alpha = 2
  )
  timesn <- max(unique(grouped_data$time))
  mesh_t <- inla.mesh.1d(
    loc = c(1:timesn),
    degree = 1,
    boundary = rep("free", 2)
  )
  

  

  
  

  ##stack###
  k <- length(local.est.cov)  # 协变量数量
  A_constr <- matrix(0, nrow = 1, ncol = k) 
  A_constr[1, ] <- 1  # 所有协变量系数之和为1
  constr_list <- list(
    A = A_constr,
    e = 1
  )
  
  
  s.index<-inla.spde.make.index(name="st.field",
                                n.spde = loc.spde$n.spde,
                                n.group = mesh_t$m)
  
  
  A.est <-  inla.spde.make.A(mesh = mesh,
                             loc = as.matrix(grouped_data[,c("x","y")]),
                             group=grouped_data$group,
                             group.mesh = mesh_t)
  
  dim(A.est) 
  stack <- inla.stack(tag="est",
                      data = list(Y.elect=grouped_data$Y.elect,
                                  wts=grouped_data$weight),
                      A=list(A.est,1),
                      effects=list(c(s.index,list(Intercept=1)),
                                   c(local.est.cov))
  )
  
  
  cat("Start processing,step2...\n\n")
  #------------step2-------------------
  # Combine the base formula and the selected sub-model components together
  base_formula <- "Y.elect ~ -1 + Intercept"
  st_term <- "f(st.field, model=loc.spde, group=st.field.group, control.group=list(model='ar1'))"
  submodel_components <- lapply(model_names, function(model_name) {
    switch(model_name,
           "las" = "f(las, model='clinear',range=c(0,1),initial=-0.2)",
           "gam" = "f(gam, model='clinear', range=c(0, 1), initial=-0.2)",
           "svm" = "f(svm, model='clinear', range=c(0, 1), initial=-0.2)",
           "rf" = "f(rf, model='clinear', range=c(0, 1), initial=0.2) ",
           "")
  }) %>% unlist() %>% 
    paste(.,collapse = " + ")
  
  formula_string <- paste(base_formula, st_term ,submodel_components, sep=" + ")
  formula <- as.formula(formula_string)
  
  
  cat("Done..........\n\n")
  #--------------done-----------------
  time_b <- Sys.time()
  cat("Total time:\n")
  cat(as.numeric(difftime(time_b, time_a, units = "secs")), "seconds\n")
  
  
  return(
    list(
      spde = loc.spde,
      s.index = s.index,
      A = A.est,
      local.est.cov = local.est.cov,
      stack = stack,
      formula_string = formula_string,
      formula = formula,
      constr_list=constr_list
    )
  )
}