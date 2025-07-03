suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(require(dplyr))
suppressMessages(require(maptools))
suppressMessages(require(rgdal))
suppressMessages(require(raster))
suppressMessages(require(caret))
suppressMessages(library(sf))
suppressMessages(library(readr))

emplogit <- function(posi, N) {
  # 计算比例
  tmp <- posi / N
  
  # 处理极端值，使用更小的值来避免 log(0)
  epsilon <- 0.0001
  top <- ifelse(tmp == 0, epsilon, tmp)
  bottom <- ifelse(tmp == 1, epsilon, 1 - tmp)
  
  # 计算并返回 logit 值
  return(log(top / bottom))
}


data_process_function <- function(shp_sf,data,n,N){
  time_a <- Sys.time()
  cat("Data checking in progress...\n\n")
  #check the classification 
  if (inherits(data, "sf") && !inherits(shp_sf, "sf")) {
    a <- shp_sf
    shp_sf <- data
    data <- a
    rm(a)
  }
  if (!inherits(shp_sf, "sf")) {
    stop("The input datas are invalid. Please provide an sf object.")
  }
  cat("The classifications of input data are valid, going on...\n\n")
  
  
  cat("Start processing, step 1...\n\n")
  shp_sf_proj <- shp_sf %>% 
    st_make_valid() %>% ###make valid
    st_transform("EPSG:4326")
  
  
  cat("Start processing, step 2...\n\n")
  #data$E_N <- data$E_1+data$E_2
  data$Y.elect <- emplogit(data[,n] %>% unlist(), data[,N] %>% unlist()) %>% as.numeric()
  
  na_y <- which(is.na(data$Y.elect))
  data$time <- data$Interview_year - min(data$Interview_year) + 1
  cov.name <- colnames(data)[16:26 ]
  cov.name <- cov.name[-which(cov.name=="distance")]
  cat("The covariates are:\n")
  print(cov.name)
  na_cov <- lapply(cov.name, function(x){which(is.na(data[,x]))}) %>% 
    do.call("c",.)
  na_all <- unique(c(na_cov,na_y))
  data_censored <- data.frame()
  if(length(na_all)>0){
    data_censored <- data[na_all,]
    data <- data[-na_all,]
  }
  
  
  data <- data[which(data$Survey%in%c("DHS6","DHS7","DHS8")),]
  data <- data[which(data$HEP_E>=5),]
  
  samp <- sample(1:nrow(data),nrow(data),replace=F)###randomize data
  data <- data[samp,]
  
  preProcess_range_model <- preProcess(data[,cov.name], method='range') #scale the covariates
  data[,cov.name] <- predict(preProcess_range_model, newdata = data[,cov.name])
  
  
  sf_id <- data %>% 
    st_as_sf(coords = c("x", "y"), crs = 4326)
  
  
  cat("Start processing, step 4...\n\n")
  #process_sf <- st_join(sf_id, shp_sf_proj, join = st_intersects)
  #the data is prepared at initial
  process_sf <- sf_id
  n_cluster <- length(unique(process_sf$No))
  n_year <- length( unique(process_sf$Interview_year))
  #zones <- unique(shp_sf_proj$REGION)
  process_data <- process_sf %>% 
    st_drop_geometry()
  process_data_coord <- process_sf %>% 
    st_coordinates() %>% 
    as.data.frame(stringsAsFactors = F)
  colnames(process_data_coord) <- c("x","y")
  process_data <- cbind(process_data,process_data_coord)
  
  
  cat("Done..........\n\n")
  time_b <- Sys.time()
  cat("Total time:\n")
  cat(as.numeric(difftime(time_b, time_a, units = "secs")), "seconds\n")
  
  return(
    list(    processed_data = process_data,
             processed_sf = process_sf,
             africa_shp = shp_sf_proj,
             n_cluster = n_cluster,
             preProcess_range_model = preProcess_range_model,
             n_year = n_year,
             #zones = zones,
             cov.name = cov.name,
             data_censored = data_censored
    )
  )
}

