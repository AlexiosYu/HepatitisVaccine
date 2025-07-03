rm(list = ls());gc()
library(sf);library(magrittr)
setwd("~/Yu/project/")
load("./data/dataall_0508.Rdata")
africa_shp <- sf::read_sf("/d2/home/guest3/Malaria/whz/Africa_0.shp") %>% 
  st_make_valid()
#csv_list <- list.files(pattern = ".csv",path = "./covar/",full.names = T)

#regularize and process it
load("~/Yu/project/Rdata/pred_csv0923.Rdata")

source("./code/RegularizeProcess.R")

processed <- data_process_function(data=data_all,shp_sf=africa_shp,n = "HEP1_1",N = "HEP_E")
processed$processed_data %>% nrow()
save(processed,file = sprintf("./Rdata/processed_%s.Rdata","HEP1_1"))

source("./code/makePolygon.R")
poly_res <- ploy_process_function(processed)

source("./code/ml_function.R")
ml_res <- sub_model_function(processed, nfolds = 5,methods =c("glmnet", "gam",  "ranger"))
gc();
cor_resdata <- ml_res$correlations
model_names <- c("las","gam","rf")

#building the project matrix and stacking with independent vars generated from sub-models 
source("./code/stack_synthesizer.R")
source("./code/output_calc.R")

stack_funsion <- stack_synthesizer_function(poly_res$mesh, ml_res,processed,model_names)
#theta <- c(-2.3748269,  1.9707168 ,-0.2767401 , 2.0980461 ,-2.3848801 ,-2.3875261,  2.5803235)
output <- output_calc_function(stack_funsion)
write.csv(output$summary,"zhep1.csv")
output$summary
output$output$mode$theta %>% unlist() %>% as.vector()
#
res <- list(
  poly_res=poly_res,
  ml_res=ml_res,
  cor_resdata=cor_resdata,
  stack_funsion=stack_funsion,
  output=output
)
save(res,file = sprintf("./Rdata/model_%s.Rdata","HEP1"))



rm(list = ls());gc()
selected_year_index <- c(2010:2022)-2010+1
load(sprintf("./Rdata/model_%s.Rdata","HEP1"))
load(sprintf("./Rdata/processed_%s.Rdata","HEP1_1"))


source("./code/pred_hyperparameter.R")
source("./code/parallel_extract_function_WGS4326.R")
source("./code/model_linkage_WGS4326.R")
source("./code/spatial_extraction.R")
source("./code/ml_predcsv.R")
model_names <- c("las","gam","rf")


res_predcsv <- ml_predcsv(processed,res,selected_year_index)
hyper_result <- pred_hyperparameter(res,res_predcsv,model_names = model_names,Num = 1000,selected_year_index = selected_year_index)
# save(hyper_result,file = sprintf("./Rdata/hyper_result_%s.Rdata","HEP1_1"))
# load(sprintf("./Rdata/hyper_result_%s.Rdata","HEP1_1"))

spatial_result <- spatial_extraction(selected_year_index,hyper_result,model_names,ncluster = 2)
save(spatial_result,file = "./Rdata/predpost_hep1.Rdata")
rm(list = c("processed","res","res_predcsv","hyper_result"));gc()

load("./Rdata/predpost_hep1.Rdata")
for (i in selected_year_index[8:13]) {
  time_2 <- Sys.time()
  yr <- selected_year_index[i]
  time_extraction <- spatial_result[[i]]
  # table_property_column <- time_extraction$table_property_column
  # save(table_property_column,file = "./Rdata/table.Rdata")
  ncluster <- 2
  local_shp <- para_fine(ncluster,time_extraction)
  
  pred_dis_list <- para_poly(ncluster, time_extraction, c("con_id", "pro_id", "dis_id"),yr)
  gc()
  pred_pro_list <- para_poly(ncluster, time_extraction, c("con_id", "pro_id"),yr)
  gc()
  pred_con_list <- para_poly(ncluster, time_extraction, c("con_id"),yr)
  gc()
  time_3 <- Sys.time()
  sprintf("Time spent on process 2: %.2fs \n",time_3-time_2) %>% cat()
  p_pred <- cbind(time_extraction$table_property_column,time_extraction$pred)
  
  write_name <- sapply(1:4, function(j){
    sprintf("./shape/%s/%s_%s%s","D1",c("5km","dis","pro","con")[j],selected_year_index[i] + 2009,c(".shp",".csv",".csv",".csv")[j])
  })
  
  write_sf(local_shp,dsn=write_name[1])
  write_csv(pred_dis_list$pred_result,file = write_name[2])
  write_csv(pred_pro_list$pred_result,file = write_name[3])
  write_csv(pred_con_list$pred_result,file = write_name[4])
  
  
  list_1000  <-  list(
    pred = time_extraction$pred,
    pred_dis = pred_dis_list$pred_result_1000,
    pred_pro = pred_pro_list$pred_result_1000,
    pred_con = pred_con_list$pred_result_1000
  )
  write_Rdata <-  sprintf("./Rdata/%s/pred_1000_%s.Rdata","D1",(selected_year_index[i]  + 2009))
  cat(sprintf("print:::::::::./Rdata/pred_1000_%s.Rdata",selected_year_index[i]))
  save(list_1000,file = write_Rdata)
  time_4 <- Sys.time()
  sprintf("Time spent on process 2: %.2fs \n",time_4-time_3) %>% cat()
  sprintf("Time spent on total process: %.2fs \n",time_4-time_2) %>% cat()
  rm(list = c("list_1000","time_extraction","pred_dis_list","pred_pro_list","pred_con_list"))
  gc()
}

#list_1000 <- spatial_result$list_1000
# rm(spatial_result);gc()
# save(list_1000,file = write_Rdata)
rm(list = ls());gc()
gc()
