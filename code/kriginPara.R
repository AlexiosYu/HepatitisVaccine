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

# for (i in 1:length(countryName)) {
#   sp.grid <- mget(countryName[i], envir = country_data)[[1]]
#   # 设置分块大小（根据内存和网格规模调整）
#   chunk_size <- 500  # 每个任务处理1000个网格点
#   total_points <- length(sp.grid)
#   chunks <- split(1:total_points, ceiling(1:total_points / chunk_size))
#   for (j in 1:6) {
#     # ------------------------- 6个变量模型 -------------------------
#     sp.dhs <- mget(dataVec[j], envir = dhs_data)[[1]] %>% 
#       st_as_sf() %>% 
#       st_transform(32634) %>% 
#       dplyr::filter(!is.na(.data[[varVec[j]]])) 
#     
#     sdhs_sp <- as(sp.dhs, "Spatial")
#     fit_vgm <- vgf_fun[[j]]
#     target_crs <- "+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs"
#     
#     # 确保坐标系一致性
#     sdhs_sp <- spTransform(sdhs_sp, CRS(target_crs))
#     sp.grid <- spTransform(sp.grid, CRS(target_crs))
#     # 
#     system.time({
#       # 创建集群
#       cl <- makeForkCluster(10)
#       registerDoParallel(cl)
#       
#       # 导出变量到集群
#       clusterExport(cl, varlist = c("varVec", "sdhs_sp", "sp.grid", "chunks", "fit_vgm", "j"))
#       
#       # 使用 tryCatch 确保 stopCluster 总是被执行
#       result <- tryCatch({
#         foreach(k = seq_along(chunks)) %dopar% {
#           num <- chunks[[k]]
#           sp.grid.chunk <- sp.grid[num, ]
#           
#           
#           # 执行克里金插值
#           krig_result <- gstat::krige(
#             as.formula(glue::glue("{varVec[j]} ~ 1")), 
#             locations = sdhs_sp, 
#             newdata = sp.grid.chunk, 
#             nmax = 400,
#             model = fit_vgm
#           )
#           
#           return(krig_result)
#         }
#       }, error = function(e) {
#         message("Error occurred: ", conditionMessage(e))
#         NULL  
#       }) 
#       
#       # 停止集群（无论如何都会执行）
#       stopCluster(cl)
#     })
#     # 创建一个栅格模板
#     template_raster <- raster(
#       extent(sp.grid),  # 使用已知数据的空间范围
#       resolution = c(5000, 5000)  # 设置栅格分辨率
#     )
#     krig_result <- result %>% do.call("rbind",.)
#     krig_raster <- rasterize(
#       krig_result,         # 插值结果的空间点
#       template_raster,     # 栅格模板
#       field = "var1.pred"  # 指定要转换的字段（预测值列名）
#     )
#     writeRaster(krig_raster, filename = sprintf("%s%s/%s.tiff",base_path,varVec[j],countryName[i]), format = "GTiff", overwrite = TRUE)
#     message(rep(sprintf("%s's %s.tiff is over!\n",varVec[j],countryName[i]),3))
#   }
#   
# }
# ------------------------- 交叉验证函数 -------------------------
# ------------------------- 修改后的交叉验证函数 -------------------------
kfold_cross_validate <- function(i) {
  var <- varVec[i]
  data <- dataVec[i]
  
  sp <- mget(data, envir = dhs_data)[[1]] %>% 
    st_as_sf() %>% 
    dplyr::filter(!is.na(.data[[var]])) %>% 
    as_Spatial()
  
  sp <- as(sp, "SpatialPointsDataFrame")
  
  set.seed(123)
  k <- 5
  folds <- sample(rep(1:k, length.out = nrow(sp)))
  
  predictions <- numeric(nrow(sp))
  
  for (fold in 1:k) {
    test_idx <- which(folds == fold)
    train <- sp[-test_idx, ]
    test <- sp[test_idx, ]
    
    variogram_train <- variogram(as.formula(paste(var, "~ 1")), train)
    fit_vgm <- tryCatch(
      fit.variogram(variogram_train, vgm(c("Exp", "Sph", "Gau")), fit.method = 7),
      error = function(e) NULL
    )
    
    if (is.null(fit_vgm)) next
    
    krig_result <- krige(
      as.formula(paste(var, "~ 1")),
      locations = train,
      newdata = test,
      model = fit_vgm,
      nmax = 400
    )
    
    predictions[test_idx] <- krig_result$var1.pred
  }
  
  actual <- sp[[var]]
  valid_idx <- !is.na(predictions)  # 过滤无效预测
  
  # 计算所有指标
  residuals <- actual[valid_idx] - predictions[valid_idx]
  rmse <- sqrt(mean(residuals^2))
  mae <- mean(abs(residuals))
  corr <- cor(actual[valid_idx], predictions[valid_idx])
  rsq <- corr^2  # 等同于1 - (sum(residuals^2)/sum((actual[valid_idx]-mean(actual[valid_idx]))^2))
  
  return(list(
    var = var,
    rmse = rmse,
    mae = mae,
    corr = corr,
    rsq = rsq,
    n_valid = sum(valid_idx),
    predictions = predictions,
    actual = actual
  ))
}

# ------------------------- 执行并行交叉验证 -------------------------
# 设置并行计算
cl <- makeCluster(6)
registerDoParallel(cl)

# 进行交叉验证（每个变量并行）
cv_results <- foreach(i = 1:6, .packages = c("gstat", "sp", "sf", "dplyr")) %dopar% {
  kfold_cross_validate(i)
}

# 停止集群
stopCluster(cl)


# ------------------------- 生成汇总表格 -------------------------
# 提取评估指标
metrics_table <- map_df(cv_results, ~{
  data.frame(
    Variable = .x$var,
    RMSE = round(.x$rmse, 4),
    MAE = round(.x$mae, 4),
    Corr = round(.x$corr, 4),
    R_Squared = round(.x$rsq, 4),
    N_Valid = .x$n_valid,
    stringsAsFactors = FALSE
  )
})

# 打印美观表格
knitr::kable(
  metrics_table,
  col.names = c("变量", "RMSE", "MAE", "相关系数", "R平方", "有效样本量"),
  caption = "空间插值模型交叉验证结果",
  align = c("l", rep("c", 5)),
  format.args = list(drop0trailing = TRUE)
) %>% 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

# 保存结果
write.csv(metrics_table, paste0(base_path, "validation_metrics.csv"), row.names = FALSE)
save(cv_results, file = paste0(base_path, "cross_validation_results.Rdata"))