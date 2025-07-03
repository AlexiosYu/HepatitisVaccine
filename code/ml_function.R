suppressMessages(library(doParallel))
suppressMessages(library(cvTools))
suppressMessages(library(parallel))
suppressMessages(library(mgcv))
suppressMessages(library(ranger))
# Function to calculate cross-validated predictions for a given method
cv_predict <- function(method, formula, data, fold_index,methods=c("glmnet", "gam", "ranger")) {
  train <- data %>% filter(nfolds != fold_index)
  valid <- data %>% filter(nfolds == fold_index)
  weight <- train$weight
  
  model <- switch(method,
                  "glmnet" = train(formula, data = train, method = method, weights = weight),
                  "gam" = gam(formula, data = train, weights = weight),
                  "svmRadial" = train(formula, data = train, method = method, weights = weight),
                  "ranger" = train(formula, data = train, method = method, weights = weight)
  )
  
  predictions <- predict(model, newdata = valid) %>% as.numeric()
  Y.elect <- valid$Y.elect
  FID=valid$FID
  dt <- data.frame(FID=FID,predictions=predictions,obs=Y.elect)
  return(dt)
}

sub_model_function <- function(processed,  nfolds = 5, num_cl = 5,methods) {
  time_a <- Sys.time()
  list_data <- processed
  cat("Start processing, step 1...\n\n")
  ####extract data
  data_all <- list_data$processed_data
  data_all <- data_all[which(!is.na(data_all$Y.elect)),]
  cov.name <- list_data$cov.name
  cat("The covariates used for Machine-Learning models are:\n")
  covariate.names <- c("time", cov.name)
  print(covariate.names)
  
  folds_vec <- cvFolds(nrow(data_all), nfolds)
  data_all$nfolds <- folds_vec$which %>% as.numeric()
  
  cat("Start processing, step 2...\n\n")
  ####Machine-Learning models
  formula <- as.formula(paste0('Y.elect ~', paste0(covariate.names, collapse = '+')))
  
  # Initialize the cross-validated predictions(cv.las,.......)
  cv_predictions <- list()
  # Loop over the methods and perform cross-validation
  for (fold_index in 1:nfolds) {
    for (method in methods) {
      cv_predictions[[method]] <- rbind(cv_predictions[[method]], cv_predict(method, formula, data_all, fold_index))
    }
  }
  # Calculate correlations for each method
  correlations <- lapply(methods, function(j) {
    dat <- cv_predictions[[j]]
    cor <- cor(plogis(dat$obs), plogis(dat$predictions))
    data.frame(name=j,correlation=cor,stringsAsFactors = F)
  }) %>% do.call("rbind",.)
  
  # Fit models on the entire pred_dataset and calculate fitted values
  models <- lapply(methods, function(method){
    switch(method,
           "glmnet" = train(formula, data = data_all, method = method, weights = weight),
           "gam" = gam(formula, data = data_all, weights = weight),
           "svmRadial" = train(formula, data = data_all, method = method, weights = weight),
           "ranger" = train(formula, data = data_all, method = method, weights = weight)
    )
  })
  
  # Calculate fitted values for the full dataset
  

  
  return(
    list(
      cv_predictions = cv_predictions,
      correlations = correlations,
      models = models,
      data_all = data_all
    )
  )
}