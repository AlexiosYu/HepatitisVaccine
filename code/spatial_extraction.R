suppressMessages(library(purrr))
suppressMessages(library(parallel))
suppressMessages(library(doParallel))
suppressMessages(library(ggplot2))



spatial_extraction <- function(selected_year_index,hyper_result,model_names,ncluster){
  
  time_0 <- Sys.time()
  cat("Start processing,step1 extraction of all years data...\n\n")
  single_list <- lapply(selected_year_index, function(time_i){
    extracted_result <- model_linkage(idx_year = time_i,hyper_result,model_names)
    return(extracted_result)
  })

  time_1 <- Sys.time()
  sprintf("Time spent on process 1: %.2fs \n",time_1-time_0) %>% cat()
  return(single_list)
  

}