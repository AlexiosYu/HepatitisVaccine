rm(list = ls());gc()
library(lwgeom)
library(sf)
library(gstat)
library(sp)
library(purrr)
library(foreach)
library(doParallel)
library(raster)
crop_function <- function(rasterA,rasterB){
  center_points <- rasterToPoints(rasterA, spatial = TRUE)  # 返回 SpatialPointsDataFrame
  
  # 转换为 sf 对象
  center_sf <- st_as_sf(center_points) %>% 
    st_set_crs(crs(rasterA))
  # 提取 rasterB 的值
  extracted_values <- extract(rasterB, center_sf)
  
  # 将提取的值添加到 sf 对象中
  center_sf$extracted_value <- extracted_values
  
  # 获取 rasterA 的行列索引
  cell_indices <- cellFromXY(rasterA, st_coordinates(center_sf))
  
  # 将提取的值赋值回 rasterA
  values(rasterA)[cell_indices] <- center_sf$extracted_value
  return(rasterA)
}
non_na_position <- function(xMatrix,yMatrix) {
  na_coords_x <- which(is.na(xMatrix), arr.ind = TRUE)
  na_coords_y <- which(is.na(yMatrix), arr.ind = TRUE)
  all_na_coords <- unique(rbind(na_coords_x, na_coords_y))
  rows_to_exclude <- unique(all_na_coords[, "row"])
  cols_to_exclude <- unique(all_na_coords[, "col"])
  rows_to_keep <- setdiff(seq_len(nrow(xMatrix)), rows_to_exclude)
  cols_to_keep <- setdiff(seq_len(ncol(xMatrix)), cols_to_exclude)
  xMatrix <- xMatrix[rows_to_keep, cols_to_keep, drop = FALSE]
  yMatrix <- yMatrix[rows_to_keep, cols_to_keep, drop = FALSE]
  return(list(xMatrix,yMatrix))
}




rdt <- list.files("./tiffRaw/", pattern = "^poss.*Rdata$", full.names = TRUE)
map(rdt, ~ load(.x, envir = .GlobalEnv))
dhsName <- c("HR","IR","KR","MA")
all_job <- lapply(1:4, function(i){
  data=dhsName[i]
  sp <- get(data)
  names <- unique(sp@data[["Country"]])
  return(names)
}) 
all_job_unique <- Reduce(intersect, all_job)
rm(list = dhsName)
shp <- read_sf("./data/Africa_2.shp")%>% 
  dplyr::filter(REIGON!="North Africa")

xList <- list.files("./tiff_part3/",pattern = "tif$",full.names = T)[1:6] %>% lapply(
  .,raster
)
xNames <- c("Anternal Care Rate","Card Rate","Information Access Rate","Insurance Rate","Medical Birth_rate","Parents Rate")
yList <- raster("./tiff_part3/y_hep3_2022.tif")



lib<-NULL
source("./GCCM/basic.r")
source("./GCCM/GCCM.r")
E=3
for (i in 4:10) {
  startTime_i<-Sys.time()
  sp <- shp %>% dplyr::filter(NAME_0==all_job_unique[i])
  # 获取空间范围
  extent_sp <- extent(sp)  # 获取sf文件的边界
  
  # 创建一个空白的栅格
  raster_empty_r <- raster(extent_sp, res = c(5000,5000),crs = crs(sp))  # res指定栅格的分辨率，例如1000代表每个像素代表1000米
  values(raster_empty_r) <- -1#暂时
  raster_crop <- crop(raster_empty_r,sp)
  raster_empty <- mask(raster_crop, sp)
  yRaster <- crop_function(raster_empty,yList)
  yName <- "Hep3 Rate"
  # plot(raster_empty)
  for (j in 4:6) {
    num_cl <- 10
    startTime<-Sys.time()
    
    r_x <- xList[[j]]
    xName <- xNames[j]
    xRaster <- crop_function(raster_empty,r_x)
    xMatrix <- as.matrix(xRaster)
    yMatrix <- as.matrix(yRaster)
    print(dim(xMatrix))
    print(dim(yMatrix))
    search_Num <- 70
    minInt <- 10 * floor(min(c(dim(xMatrix),dim(yMatrix)))  / 10) 
    if(minInt<search_Num){
      search_Num <- minInt
    }
    lower <- 10
    if(minInt/5<lower){
      lower <- round(minInt/5)
    }
    lib_sizes<-seq(lower,search_Num,length.out = 5) %>% ceiling()
    print(lib_sizes)
    # mList <- non_na_position(xMatrix,yMatrix)
    # xMatrix <- mList[[1]]
    # yMatrix <- mList[[2]]
    
    imageSize<-dim(yMatrix)
    totalRow<-imageSize[1]
    totalCol<-imageSize[2]
    
    
    predRows<-seq(5,totalRow,5)
    predCols<-seq(5,totalCol,5)
    pred<-merge(predRows,predCols)
    
    
    
    x_xmap_y <- GCCM(xMatrix, yMatrix, lib_sizes, lib, pred, E,tau = 1,b=E+2,winStepRatio = 0,cores=num_cl,dir=0)
    endTime1<-Sys.time()
    
    print(difftime(endTime1,startTime, units ="mins"))
    y_xmap_x <- GCCM(yMatrix, xMatrix, lib_sizes, lib, pred, E,tau = 1,b=E+2,winStepRatio = 0,cores=num_cl,dir=0)
    
    x_xmap_y$L <- as.factor(x_xmap_y$L)
    x_xmap_y_means <- do.call(rbind, lapply(split(x_xmap_y, x_xmap_y$L), function(x){max(0, mean(x$rho,na.rm=TRUE))}))
    
    y_xmap_x$L <- as.factor(y_xmap_x$L)
    y_xmap_x_means <- do.call(rbind, lapply(split(y_xmap_x, y_xmap_x$L), function(x){max(0, mean(x$rho,na.rm=TRUE))}))
    
    x_xmap_y_Sig<- significance(x_xmap_y_means,nrow(pred))    #Test the significance of the prediciton accuray
    y_xmap_x_Sig<- significance(y_xmap_x_means,nrow(pred))     #Test the significance of the prediciton accuray
    
    x_xmap_y_interval<- confidence(x_xmap_y_means,nrow(pred))
    colnames(x_xmap_y_interval)<-c("x_xmap_y_upper","x_xmap_y_lower")   #calculate the  95%. confidence interval  of the prediciton accuray
    
    y_xmap_x_interval<- confidence(y_xmap_x_means,nrow(pred))
    colnames(y_xmap_x_interval)<-c("y_xmap_x_upper","y_xmap_x_lower")  #calculate the  95%. confidence interval  of the prediciton accuray
    
    len <- min(sapply(list(lib_sizes,x_xmap_y_means,y_xmap_x_means,x_xmap_y_Sig,y_xmap_x_Sig,x_xmap_y_interval,y_xmap_x_interval),length))
    results<-data.frame(lib_sizes[1:len],x_xmap_y_means[1:len],y_xmap_x_means[1:len],x_xmap_y_Sig[1:len],y_xmap_x_Sig[1:len],x_xmap_y_interval[1:len],y_xmap_x_interval[1:len])  #Save the cross-mapping prediciton results
    write.csv(results, file=sprintf("./gccmResult/%s_%s VS %s.csv",all_job_unique[i],yName,xName))
    par(mfrow=c(1,1))
    par(mar=c(5, 4, 4, 2) + 0.1)
    
    jpeg(filename = sprintf("./gccmResult/jpg/%s_%s VS %s.jpg",all_job_unique[i],yName,xName),width = 600, height = 400)
    lsz <- lib_sizes[1:len]
    plot(lsz, x_xmap_y_means, type = "l", col = "royalblue", lwd = 2,
         xlim = c(min(lsz), max(lsz)), ylim = c(0.0, 1), xlab = "L", ylab = expression(rho))
    lines(lsz, y_xmap_x_means[1:len], col = "red3", lwd = 2)
    # legend(min(lib_sizes), 1, legend = c("x xmap y", "y xmap x"),
    #        xjust = 0, yjust = 1, lty = 1, lwd = 2, col = c("royalblue", "red3"))
    legend(min(lsz), 1, legend = c(paste(xName,"xmap",yName,sep=" "), paste(yName,"xmap",xName,sep=" ")), 
           xjust = 0, yjust = 1, lty = 1, lwd = 2, col = c("royalblue", "red3"))
    dev.off()
    
    endTime<-Sys.time()
    
    print(difftime(endTime,startTime, units ="mins"))
    gc()
  }
  endTime_i<-Sys.time()

  print(difftime(endTime_i,startTime_i, units ="mins"))
  cat(rep(sprintf("The %s vs %s in %s is over!\n",yName,xName,all_job_unique[i]),3))
}
