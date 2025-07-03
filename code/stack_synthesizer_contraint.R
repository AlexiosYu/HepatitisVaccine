suppressMessages(require(dplyr))
suppressMessages(require(INLA))
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
  loc.spde <- inla.spde2.matern(
    mesh=mesh, alpha=1.5,
    prior.pc.rho = c(0.3, 0.5),
    prior.pc.sig = c(0.5, 0.5),
    alpha        = 2)
  grouped_data <- ml_res$cv_predictions$glmnet %>% 
    mutate(group = time - min(time) + 1 )
  timesn <- max(unique(grouped_data$time))
  mesh_t <- inla.mesh.1d(
    loc = c(1:timesn),
    degree = 1,
    boundary = rep("free", 2)
  )
  s.index<-inla.spde.make.index(name="st.field",
                                n.spde = loc.spde$n.spde,
                                n.group = timesn)


  
  gbd_sd <- (grouped_data$gbd_upper - grouped_data$gbd_lower) / 3.92  # 转换为标准差
  hyper.country <- list(
    prec = list(
      prior = "loggamma",
      param = c(1, 1 / mean(gbd_sd, na.rm = TRUE)^2)  # 精度=1/方差
    )
  )
  A.est <-  inla.spde.make.A(mesh = mesh,
                             loc = as.matrix(grouped_data[,c("x","y"),with=F]),
                             group=grouped_data$group,
                             group.mesh = mesh_t)
  
  dim(A.est) 
  
  model_pred <- lapply(ml_res$cv_predictions, function(x){
    dat <- x[,"predictions"]
  })
  Y.elect <-  grouped_data$obs
  ##select submodel###
  model_names <- unique(model_names)
  selected_submodels <- lapply(model_names, function(x) {
    switch(x,
           "las" = model_pred$glmnet,
           "gam"   = model_pred$gam,
           "svm"   = model_pred$svmRadial,
           "rf"    = model_pred$ranger,
           NULL)
  })
  local.est.cov <- setNames(selected_submodels , model_names)
  grouped_data$con_id <- as.factor(grouped_data$con_id)
  ##stack###
  stack <- inla.stack(tag="est",
                      data = list(Y.elect=Y.elect,
                                  wts=grouped_data$weight),
                      A=list(A.est,1,1),
                      effects=list(c(s.index,list(Intercept=1)),
                                   c(local.est.cov),
                                   list(con_id = grouped_data$con_id))
  )
  
  
  cat("Start processing,step2...\n\n")
  #------------step2-------------------
  # Combine the base formula and the selected sub-model components together
  base_formula <- "Y.elect ~ -1 + Intercept"
  st_term <- "f(st.field, model=loc.spde, group=st.field.group, control.group=list(model='ar1'))"
  country_term <- "f(con_id, model = 'iid', hyper = hyper.country, constr = TRUE) "
  submodel_components <- lapply(model_names, function(model_name) {
    switch(model_name,
           "las" = "f(las, model='clinear',range=c(0,1),initial=-0.2)",
           "gam" = "f(gam, model='clinear', range=c(0, 1), initial=-0.2)",
           "svm" = "f(svm, model='clinear', range=c(0, 1), initial=-0.2)",
           "rf" = "f(rf, model='clinear', range=c(0, 1), initial=0.2) ",
           "")
  }) %>% unlist() %>% 
    paste(.,collapse = " + ")
  
  formula_string <- paste(base_formula, st_term ,submodel_components,country_term, sep=" + ")
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
      formula = formula
    )
  )
}