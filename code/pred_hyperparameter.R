suppressMessages(library(sf))
suppressMessages(library(INLA))
pred_hyperparameter <- function(res,ml_predcsv,model_names,Num = 1000,selected_year_index){
  time_a <- Sys.time()
  ml_res <- ml_predcsv
  sub_model_result <- res
  selected_year <- selected_year_index + 2009
  all_pred_csv <- ml_res$pred_csv
  sub_pred_csv <- all_pred_csv %>% 
    filter(B %in% selected_year)
  rm(all_pred_csv);gc()
  #------------step1-------------------
  ### produce the proj matrix
  # pred_loc <- sub_pred_csv %>% 
  #   dplyr::select(X,Y) %>% 
  #   st_as_sf(coords=c("X","Y"),crs=32634) %>% 
  #   st_transform(4326) %>% 
  #   st_coordinates()
  
  pred_loc <- sub_pred_csv %>% 
    dplyr::select(LONNUM,LATNUM) %>% 
    st_as_sf(coords=c("LONNUM","LATNUM"),crs=4326)  %>% 
    st_coordinates()
  
  group_p <- sub_pred_csv$time
  node_t <- unique(sub_pred_csv$time)
  mesh_t <- inla.mesh.1d(
    loc = node_t,
    degree = 1,
    boundary = rep("free", 2)
  )
  A.pred <- inla.spde.make.A(mesh=sub_model_result$poly_res$mesh,
                             loc=as.matrix(pred_loc), 
                             group=group_p,
                             group.mesh = mesh_t
  )
  
  #------------step2-------------------
  ### predict X model
  #sub_pred_csv_i <- sub_pred_csv[1:10,]#FOR TESTING
  
  
  local.pred.cov <- ml_res$fit
  names(local.pred.cov) <- model_names
  
  #------------step3-------------------
  ### sampling the posterior distribution for the parameter
  
  draws <- inla.posterior.sample(Num, sub_model_result$output$output)
  draws_hyp <- inla.hyperpar.sample(Num,sub_model_result$output$output)
  par_names <- rownames(draws[[1]]$latent)
  x <- sapply(par_names,function(x) unlist(strsplit(x ,split='[:]'))[1])
  #table(x)
  s_idx <- grep('st.field', par_names) ###index for the mesh 
  pred_s <- sapply(draws, function (x) x$latent[s_idx])
  int_idx <- grep("Intercept", par_names)
  Intercept_vec <- sapply(1:Num, function(i){
    draws[[i]]$latent[int_idx]
  }) 
  
  cat("Done..........\n\n")
  time_b <- Sys.time()
  cat("Total time:\n")
  cat(as.numeric(difftime(time_b, time_a, units = "secs")), "seconds\n")
  
  cat(sprintf("the dim of A.pred is: \n%d \t %d",dim(A.pred)[1],dim(A.pred)[2]))
  cat(sprintf("the dim of pred_s is: \n%d \t %d",dim(pred_s)[1],dim(pred_s)[2]))
  return(
    list(
      A = A.pred,
      draws_hyp = draws_hyp,
      par_names = par_names,
      local.pred.cov = local.pred.cov,
      Intercept_vec = Intercept_vec,
      pred_s = pred_s,
      pred_csv = sub_pred_csv
    )
  )
}
