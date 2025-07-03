suppressMessages(library(INLA))
ploy_process_function <- function(input_list){
  time_a <- Sys.time()
  
  cat("Start processing, step 1...\n\n")
  local.region <- input_list$africa_shp #extract local region
  local.region.buffer <- st_buffer(local.region,dist=1) #a buffer of 1 degree around the region
  local.region.buffer.sp <- local.region.buffer %>% 
    as_Spatial()
  bder <- inla.sp2segment(local.region.buffer.sp) #use buffer as the border
  bder$loc <- inla.mesh.map(bder$loc)
  myboundary <- inla.nonconvex.hull(bder$loc,convex=-0.2)
  
  cat("Start processing, step 2...\n\n")
  data_sf_i <- input_list$processed_sf#extract local obs
  st_geometry(data_sf_i) <- "geometry"
  coords <- data_sf_i %>% 
    st_coordinates() %>% 
    as.data.frame()
  
  cat("Start processing, step 3...\n\n")
  local.mesh <- inla.mesh.2d(boundary  = bder,
                             loc=cbind(coords$X,coords$Y),
                             cutoff = 0.5,#for a fine mesh
                             max.edge = c(0.5,5),
                             offset = c(0.5,5))
  
  
  cat("Done..........\n\n")
  time_b <- Sys.time()
  cat("Total time spent on processing polygon data:\n")
  cat(as.numeric(difftime(time_b, time_a, units = "secs")), "seconds\n")
  
  return(
    list(
      #region_i = region_i,
      local.region.buffer.sp = local.region.buffer.sp,
      data_sf_i = data_sf_i,
      coords_i = coords,
      mesh = local.mesh
    )
  )
}