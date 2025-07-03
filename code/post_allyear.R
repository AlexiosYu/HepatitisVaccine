rm(list = ls());gc()
selected_year_index <- c(2010:2022)-2010+1
load(sprintf("./Rdata/model_%s.Rdata","HEP3"))
load(sprintf("./Rdata/processed_%s.Rdata","HEP3_1"))
source("./code/pred_hyperparameter.R")
source("./code/parallel_extract_function_WGS4326.R")
source("./code/model_linkage_WGS4326.R")
source("./code/spatial_extraction.R")
source("./code/ml_predcsv.R")
model_names <- c("las","gam","rf")


res_predcsv <- ml_predcsv(processed,res,selected_year_index)
hyper_result <- pred_hyperparameter(res,res_predcsv,model_names = model_names,Num = 1000,selected_year_index = c(2010:2022)-2010+1)
spatial_result <- spatial_extraction(selected_year_index,hyper_result,model_names,ncluster = 2)

rm(list = c("processed","res","res_predcsv","hyper_result"));gc()
for (i in 1:length(selected_year_index)) {
  time_2 <- Sys.time()
  yr <- selected_year_index[i]
  time_extraction <- spatial_result[[i]]
  # table_property_column <- time_extraction$table_property_column
  # save(table_property_column,file = "./Rdata/table.Rdata")
  ncluster <- 5
  local_shp <- para_fine(ncluster,time_extraction)
  time_3 <- Sys.time()
  sprintf("Time spent on process 2: %.2fs \n",time_3-time_2) %>% cat()
  p_pred <- cbind(time_extraction$table_property_column,time_extraction$pred)
  
  write_name <- sprintf("./shape/%s/%s_%s%s","D3","5km",selected_year_index[i] + 2009,".shp")
 
  
  write_sf(local_shp,dsn=write_name[1])

  
  

  cat(sprintf("print:::::::::./Rdata/pred_1000_%s.Rdata",selected_year_index[i]))
  time_4 <- Sys.time()
  sprintf("Time spent on process 2: %.2fs \n",time_4-time_3) %>% cat()
  sprintf("Time spent on total process: %.2fs \n",time_4-time_2) %>% cat()
  gc()
}
