rm(list = ls());gc()
load(sprintf("./Rdata/diff/diff_%s.Rdata",2010-2009))
l2010 <- lll
load(sprintf("./Rdata/diff/diff_%s.Rdata",2022-2009))
l2022 <- lll
rm(lll)
r10 <- l2010[[1]]/l2010[[2]]
r22 <- l2022[[1]]/l2022[[2]]

d2210 <- r22-r10
cl<-makeForkCluster(5)

time1 <- Sys.time()
cat(paste(time1,"start"))
cat("\n")
xbar = parApply(cl,d2210,MARGIN=1,mean,na.rm=T)
time2 <- Sys.time()
cat(paste(time2,"mean"))
cat("\n")
s = parApply(cl,d2210,MARGIN=1,sd,na.rm=T)
time3 <- Sys.time()
cat(paste(time3,"sd"))
cat("\n")
qt <- parApply(cl,d2210, MARGIN=1, function(x) quantile(x,c(0.025,0.975),na.rm=T)) %>% t() %>% as.data.frame()
time4 <- Sys.time()
cat(paste(time4,"qt"))
cat("\n")
df.final <- data.frame(l2022[[3]],
                       diff = xbar, 
                       sd = s,
                       low = qt$`2.5%`, 
                       up = qt$`97.5%`)
result_shp <- st_as_sf(df.final, coords = c(x = "X", y = "Y"),crs=st_crs(32634))
stopCluster(cl)
write_sf(result_shp,"./shape/5km_diff_rake_2210_d1.shp")



d2210 <- (r22-r10)/r10
cl<-makeForkCluster(5)

time1 <- Sys.time()
cat(paste(time1,"start"))
cat("\n")
xbar = parApply(cl,d2210,MARGIN=1,mean,na.rm=T)
time2 <- Sys.time()
cat(paste(time2,"mean"))
cat("\n")
s = parApply(cl,d2210,MARGIN=1,sd,na.rm=T)
time3 <- Sys.time()
cat(paste(time3,"sd"))
cat("\n")
qt <- parApply(cl,d2210, MARGIN=1, function(x) quantile(x,c(0.025,0.975),na.rm=T)) %>% t() %>% as.data.frame()
time4 <- Sys.time()
cat(paste(time4,"qt"))
cat("\n")
df.final <- data.frame(l2022[[3]],
                       diff = xbar, 
                       sd = s,
                       low = qt$`2.5%`, 
                       up = qt$`97.5%`)
result_shp <- st_as_sf(df.final, coords = c(x = "X", y = "Y"),crs=st_crs(32634))
stopCluster(cl)
write_sf(result_shp,"./shape/5km_diff_rake_2210_rltv_d1.shp")