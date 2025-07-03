rm(list = ls());gc()
source("./code/pred_hyperparameter.R")
source("./code/parallel_extract_function_WGS4326.R")
source("./code/model_linkage_WGS4326.R")
source("./code/spatial_extraction.R")
source("./code/ml_predcsv.R")
source("./code/rake_functions.R")
source("./code/rake_poly_function.R")
model_names <- c("las","gam","rf")
selected_year_index <- c(2010:2022)-2010+1
hep1 <- read.csv("./data/hep1_datat111.csv")
hep1 <- hep1[which(!is.na(hep1$year)),]
hep3 <- read.csv("./data/hep3_datat.csv")[,2:5]
hep3 <- hep3[which(!is.na(hep3$year)),]
mat <- data_frame()
for (i in c(2011:2022)-2009) {
  time_2 <- Sys.time()
  load(sprintf("./Rdata/%s/pred_1000_%s.Rdata","D1",(selected_year_index[i]  + 2009)))
  shp_10 <- read_sf(sprintf("./shape/%s/5km_%s.shp","D1",(selected_year_index[i]  + 2009)))
  yr <- selected_year_index[i]
  pred <- list_1000$pred
  pred_con <- list_1000$pred_con
  adjusted_pred_1 <- data_frame()
  adjusted_tibs_1 <- data_frame()
  
  for (j in unique(pred_con$con_id)) {
    gbdval <- hep1$mean[which((hep1$con_id==j)&(hep1$year==yr+2009))]
    nlen <- which(shp_10$con_id==j)
    vals <- pred[nlen,]
    if(j %in% hep1$con_id){
      
      weights <- shp_10$population[nlen]
      K <- SimpleFindK(gbdval, vals, weights)
      adjusted_vals <- adjust_matrix(K, vals)
      
      
      
    }else{
      adjusted_vals <- vals
    }
    adjusted_pred_1 <- rbind(adjusted_pred_1,adjusted_vals)
    tibs <- cbind(shp_10[nlen,] %>% st_drop_geometry(),shp_10[nlen,] %>% st_coordinates())
    adjusted_tibs_1 <- rbind(adjusted_tibs_1,tibs)
  }
  
  
  load(sprintf("./Rdata/%s/pred_1000_%s.Rdata","D3",(selected_year_index[i]  + 2009)))
  shp_10 <- read_sf(sprintf("./shape/%s/5km_%s.shp","D3",(selected_year_index[i]  + 2009)))
  yr <- selected_year_index[i]
  pred <- list_1000$pred
  pred_con <- list_1000$pred_con
  adjusted_pred_3 <- data_frame()
  adjusted_tibs_3 <- data_frame()
  
  for (j in unique(pred_con$con_id)) {
    gbdval <- hep3$mean[which((hep3$con_id==j)&(hep3$year==yr+2009))]
    nlen <- which(shp_10$con_id==j)
    vals <- pred[nlen,]
    if(j %in% hep3$con_id){
      
      weights <- shp_10$population[nlen]
      K <- SimpleFindK(gbdval, vals, weights)
      adjusted_vals <- adjust_matrix(K, vals)
      
      
      
    }else{
      adjusted_vals <- vals
    }
    adjusted_pred_3 <- rbind(adjusted_pred_3,adjusted_vals)
    tibs <- cbind(shp_10[nlen,] %>% st_drop_geometry(),shp_10[nlen,] %>% st_coordinates())
    adjusted_tibs_3 <- rbind(adjusted_tibs_3,tibs)
  }
  adpop <- adjusted_tibs_3$population %>% unlist()
  diff <- adjusted_pred_1-adjusted_pred_3
  r_diff <- (adjusted_pred_1-adjusted_pred_3)/adjusted_pred_1
  pop_pred <- diff*adpop
  lll <- list(diff,r_diff,adjusted_tibs_3)
  save(lll,file = sprintf("./Rdata/diff/diff_%s.Rdata",yr))
  rm(list = c("lll","list_1000"))
  gc()
}
