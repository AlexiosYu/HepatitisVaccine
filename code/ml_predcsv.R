suppressMessages(library(doParallel))
suppressMessages(library(cvTools))
suppressMessages(library(parallel))
suppressMessages(library(mgcv))
suppressMessages(library(ranger))
suppressMessages(library(readr))
# Function to calculate cross-validated predictions for a given method
cv_predict <- function(method, formula, data, fold_index,methods=c("glmnet", "gam", "ranger")) {
  train <- data %>% filter(nfolds != fold_index)
  valid <- data %>% filter(nfolds == fold_index)
  weight <- train$weight
  
  model <- switch(method,
                  "glmnet" = train(formula, data = train, method = method, weights = weight),
                  "gam" = gam(formula, data = train, weights = weight,na.rm=T),
                  "svmRadial" = train(formula, data = train, method = method, weights = weight),
                  "ranger" = train(formula, data = train, method = method, weights = weight)
  )
  
  predictions <- predict(model, newdata = valid) %>% as.numeric()
  Y.elect <- valid$Y.elect
  dt <- data.frame(predictions = predictions, obs = Y.elect)
  
  return(dt)
}
ml_predcsv <- function(processed,res,selected_year_index){
  load("./Rdata/pred_csv0923.Rdata")
  time_a <- Sys.time()
  pred_csv$time <- pred_csv$B - min(processed$processed_data$Interview_year) + 1
  pred_csv <- pred_csv %>% filter(time %in% selected_year_index)
  #pred_csv <- lapply(selected_year_index, function(x) read_csv(csv_list[x])) %>% do.call("rbind",.)
  preProcess_range_model <- processed$preProcess_range_model
  
  cov.name <- processed$cov.name
  covariate.names <- c("time", cov.name)
  #id_na <- lapply(covariate.names,function(x){which(is.na(pred_csv[,x]))}) %>% do.call("c",.) %>% unique()
  #pred_csv <- pred_csv[-id_na,]
  pred_csv$population <- pred_csv$pop
  pred_csv$pop <- pred_csv$pop/25
  pred_csv[,cov.name] <- predict(preProcess_range_model, newdata = pred_csv[,cov.name])
  
  models <- res$ml_res$models
  fit <- lapply(models, function(model) {
    predict(model, newdata = pred_csv) %>% as.numeric() %>% drop()
    
  })
  methods=c("glmnet", "gam", "ranger")
  names(fit) <- methods
  cat("Done..........\n\n")
  time_b <- Sys.time()
  cat("Total time:\n")
  cat(as.numeric(difftime(time_b, time_a, units = "secs")), "seconds\n")
  return(
    list(
      fit = fit,
      pred_csv = pred_csv)
  )
}