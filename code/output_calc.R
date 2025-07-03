
output_calc_function <- function(stack_funsion,theta = NULL){
  formula_inla <- stack_funsion$formula
  stack_ <- stack_funsion$stack
  spde_ = stack_funsion$spde
  constr_list <- stack_funsion$constr_list
  inla.setOption("enable.inla.argument.weights", TRUE)
  inla.setOption(num.threads = "1:1")
  time_a <- Sys.time()
  if (!is.null(theta)) {
    output <- inla(formula_inla, data = inla.stack.data(stack_, spde=spde_),
                   control.predictor = list(A=inla.stack.A(stack_), compute = F, link = 1),
                   verbose=T, weights = stack_$data$data$wts,
                   control.compute=list(config=TRUE),
                   # control.fixed = list(
                   #   extraconstr = list(
                   #     covars = constr_list  # 应用协变量约束
                   #   )
                   # ),
                   control.mode = list(theta=theta,restart=T),
                   control.inla = list(
                     strategy = 'gaussian',  # 更精确的 Laplace 近似
                     int.strategy = 'eb',            
                     fast = TRUE,                     
                     dz = 0.75,                        # 减小步长提升稳定性
                     step.factor = 0.25, 
                     reordering = "metis",               # 更保守的优化步长
                     stupid.search = FALSE
                   )
    )
  }else{
    output <- inla(formula_inla, data = inla.stack.data(stack_, spde=spde_),
                   control.predictor = list(A=inla.stack.A(stack_), compute = F, link = 1),
                   verbose=T, weights = stack_$data$data$wts,
                   control.compute=list(config=TRUE),
                   control.inla = list(
                     strategy = 'gaussian',  
                     int.strategy = 'eb',            
                     fast = TRUE,                     
                     dz = 0.75,                        # 减小步长提升稳定性
                     step.factor = 0.25,               # 更保守的优化步长
                     stupid.search = FALSE
                   )
    )
  }
  cat("Done..........\n\n")
  time_b <- Sys.time()
  cat("Total time:\n")
  cat(as.numeric(difftime(time_b, time_a, units = "secs")), "seconds\n")
  cat("\014")
  
  sop <- summary(output)
  x <- sop$fixed[,-7]
  y <- sop$hyperpar
  z = rbind(x,y)
  View(z)
  return(
    list(
      output = output,
      summary = z
    )
  )
}
