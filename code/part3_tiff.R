# 加载所需包
library(gstat)
library(sp)
library(sf)
library(purrr)
library(foreach)
library(doParallel)
library(raster)
# 清空工作空间
rm(list = ls());gc()



# ------------------------- 基础路径 -------------------------
varVec <- c("parents_rate","AnternalCareRate","medical_birth_rate","CARD_rate","info_rate","Insurance_rate")
dataVec <- c("HR","IR","KR","KR","MA","MA")
base_path <- "./tiff_part3/"
if (!dir.exists(base_path)) {
  dir.create(base_path, recursive = TRUE)
}

# 遍历 varVec 并创建对应的子文件夹
for (var in varVec) {
  # 构造完整路径
  folder_path <- file.path(base_path, var)
  
  # 如果文件夹不存在，则创建
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    message(paste("Created folder:", folder_path))
  } else {
    message(paste("Folder already exists:", folder_path))
  }
}


# ------------------------- 数据加载与预处理 -------------------------
# 在新的环境中加载数据
dhs_data <- new.env()
rdt <- list.files("./tiffRaw/", pattern = "^poss.*Rdata$", full.names = TRUE)
walk(rdt, ~ load(.x, envir = dhs_data))

country_data <- new.env()
cdt <- list.files("./country_grid/", pattern = "Rdata$", full.names = TRUE)
walk(cdt, function(x){
  name <- strsplit(x,"grid_|.Rdata")[[1]][2]
  load(x,envir=country_data)
  country_data[[name]] <- country_data$sp.grid
  rm("sp.grid",envir=country_data)
})


# ------------------------- 变异函数计算与模型拟合 -------------------------
# 计算经验变异函数
vgf <- function(i){
  var=varVec[i]
  data=dataVec[i]
  sp <- mget(data, envir = dhs_data)[[1]] %>% 
    st_as_sf() %>% 
    dplyr::filter(!is.na(.data[[var]])) %>% 
    as_Spatial()
  variogram_fun <- variogram(as.formula(glue::glue("{var} ~ 1")), locations = sp)
  fit_vgm <- fit.variogram(variogram_fun, vgm(c("Exp", "Sph", "Gau")), fit.method = 7)
  if (attr(fit_vgm, "singular")) {
    stop("变异函数模型拟合失败，请检查数据分布和模型选择")
  }
  return(fit_vgm)
}#交叉验证 5折
vgf_fun <- lapply(1:6, function(i) vgf(i)) %>% 
  suppressWarnings()

# ------------------------- 构建预测网格 -------------------------
countryName <- cdt %>% sapply(.,function(x) strsplit(x,"grid_|.Rdata")[[1]][2])
dhsName <- c("HR","IR","KR","MA")


# ------------------------- 设置并行计算 -------------------------

for (i in 1:length(countryName)) {
  sp.grid <- mget(countryName[i], envir = country_data)[[1]]
  # 设置分块大小（根据内存和网格规模调整）
  chunk_size <- 500  # 每个任务处理1000个网格点
  total_points <- length(sp.grid)
  chunks <- split(1:total_points, ceiling(1:total_points / chunk_size))
  for (j in 1:6) {
    # ------------------------- 6个变量模型 -------------------------
    sp.dhs <- mget(dataVec[j], envir = dhs_data)[[1]] %>% 
      st_as_sf() %>% 
      st_transform(32634) %>% 
      dplyr::filter(!is.na(.data[[varVec[j]]])) 
      
    sdhs_sp <- as(sp.dhs, "Spatial")
    fit_vgm <- vgf_fun[[j]]
    target_crs <- "+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs"
    
    # 确保坐标系一致性
    sdhs_sp <- spTransform(sdhs_sp, CRS(target_crs))
    sp.grid <- spTransform(sp.grid, CRS(target_crs))
# 
    system.time({
      # 创建集群
      cl <- makeForkCluster(10)
      registerDoParallel(cl)
      
      # 导出变量到集群
      clusterExport(cl, varlist = c("varVec", "sdhs_sp", "sp.grid", "chunks", "fit_vgm", "j"))
      
      # 使用 tryCatch 确保 stopCluster 总是被执行
      result <- tryCatch({
        foreach(k = seq_along(chunks)) %dopar% {
          num <- chunks[[k]]
          sp.grid.chunk <- sp.grid[num, ]

          
          # 执行克里金插值
          krig_result <- gstat::krige(
            as.formula(glue::glue("{varVec[j]} ~ 1")), 
            locations = sdhs_sp, 
            newdata = sp.grid.chunk, 
            nmax = 400,
            model = fit_vgm
          )
          
          return(krig_result)
        }
      }, error = function(e) {
        message("Error occurred: ", conditionMessage(e))
        NULL  
      }) 
      
      # 停止集群（无论如何都会执行）
      stopCluster(cl)
    })
    # 创建一个栅格模板
    template_raster <- raster(
      extent(sp.grid),  # 使用已知数据的空间范围
      resolution = c(5000, 5000)  # 设置栅格分辨率
    )
    krig_result <- result %>% do.call("rbind",.)
    krig_raster <- rasterize(
      krig_result,         # 插值结果的空间点
      template_raster,     # 栅格模板
      field = "var1.pred"  # 指定要转换的字段（预测值列名）
    )
    writeRaster(krig_raster, filename = sprintf("%s%s/%s.tiff",base_path,varVec[j],countryName[i]), format = "GTiff", overwrite = TRUE)
    message(rep(sprintf("%s's %s.tiff is over!\n",varVec[j],countryName[i]),3))
  }

}
