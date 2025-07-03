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
mat <- data_frame()
for (i in c(2010:2022)-2009) {
  time_2 <- Sys.time()
  load(sprintf("./Rdata/%s/pred_1000_%s.Rdata","D1",(selected_year_index[i]  + 2009)))
  shp_10 <- read_sf(sprintf("./shape/%s/5km_%s.shp","D1",(selected_year_index[i]  + 2009)))
  yr <- selected_year_index[i]
  pred <- list_1000$pred
  pred_con <- list_1000$pred_con
  adjusted_pred <- data_frame()
  adjusted_tibs <- data_frame()
  
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
    adjusted_pred <- rbind(adjusted_pred,adjusted_vals)
    tibs <- cbind(shp_10[nlen,] %>% st_drop_geometry(),shp_10[nlen,] %>% st_coordinates())
    adjusted_tibs <- rbind(adjusted_tibs,tibs)
  }
  
  adpop <- adjusted_tibs$population %>% unlist()
  pop_pred <- adjusted_pred*adpop
  pop_1000 <- colSums(pop_pred)/sum(adpop)
  df.final <- data.frame(B=unique(adjusted_tibs$B),
                         mean = mean(pop_1000),
                         sd = sd(pop_1000),
                         low = quantile(pop_1000,c(0.025,0.975))[1],
                         up = quantile(pop_1000,c(0.025,0.975))[2])
  mat <- rbind(mat,pop_1000)
  colnames(mat) <- 1:1000
  write.csv(df.final,sprintf("./shape/rake/%s/%s_%s%s","D1","all",selected_year_index[i] + 2009,".csv"),row.names = F)
}
write.csv(mat,sprintf("./shape/rake/%s/%s_%s%s","D1","all","mat",".csv"),row.names = F)
