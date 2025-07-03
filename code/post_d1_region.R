
library(readr)
library(magrittr)
library(dplyr)

rm(list = ls());gc()
selected_year_index <- c(2010,2015,2019:2022)-2010+1
load(sprintf("./Rdata/model_%s.Rdata","HEP1"))
load(sprintf("./Rdata/processed_%s.Rdata","HEP1_1"))
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

dat <- data.frame()
for(i in 1:length(spatial_result)){
  yr <- selected_year_index[i] + 2009
  time_extraction <- spatial_result[[i]]
  N <- time_extraction$table_property_column$population %>% sum()
  pred <- time_extraction$pred_population
  vec <- c(yr,sapply(1:1000, function(x){
    sum(pred[,x])/N
  }))
  dat <- rbind(dat,vec)
}

colnames(dat) <- c("Year",1:1000)

write_csv(dat,file = "./shape/africa_region_d1.csv")

