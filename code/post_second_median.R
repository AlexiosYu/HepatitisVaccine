rm(list = ls())
load("./Rdata/D3/pred_1000_2022.Rdata")
l22 <- list_1000
load("./Rdata/D3/pred_1000_2019.Rdata")
l19 <- list_1000
load("./Rdata/D3/pred_1000_2015.Rdata")
l15 <- list_1000
load("./Rdata/D3/pred_1000_2010.Rdata")
l10 <- list_1000
rm(list_1000)

p22 <- l22[[2]]
p19 <- l19[[2]]
p15 <- l15[[2]]
p10 <- l10[[2]]

dat <- data.frame()


dat <- data.frame(
  p2010 = apply(p10[, 5:1004], 2, function(x) {
    median(x %>% unlist())
  }) %>% unlist(),
  p2015 = apply(p15[, 5:1004], 2, function(x) {
    median(x %>% unlist())
  }) %>% unlist(),
  p2019 = apply(p19[, 5:1004], 2, function(x) {
    median(x %>% unlist())
  }) %>% unlist(),
  p2022 = apply(p22[, 5:1004], 2, function(x) {
    median(x %>% unlist())
  }) %>% unlist()
)
apply(dat,2, function(x){
  return(c(mean(x),quantile(x,c(0.025,0.5,0.975))))
})
rm(list = ls())
load("./Rdata/D1/pred_1000_2022.Rdata")
l22 <- list_1000
load("./Rdata/D1/pred_1000_2019.Rdata")
l19 <- list_1000
load("./Rdata/D1/pred_1000_2015.Rdata")
l15 <- list_1000
load("./Rdata/D1/pred_1000_2010.Rdata")
l10 <- list_1000
rm(list_1000)

p22 <- l22[[2]]
p19 <- l19[[2]]
p15 <- l15[[2]]
p10 <- l10[[2]]

dat <- data.frame()


dat <- data.frame(
  p2010 = apply(p10[, 5:1004], 2, function(x) {
    median(x %>% unlist())
  }) %>% unlist(),
  p2015 = apply(p15[, 5:1004], 2, function(x) {
    median(x %>% unlist())
  }) %>% unlist(),
  p2019 = apply(p19[, 5:1004], 2, function(x) {
    median(x %>% unlist())
  }) %>% unlist(),
  p2022 = apply(p22[, 5:1004], 2, function(x) {
    median(x %>% unlist())
  }) %>% unlist()
)
apply(dat,2, function(x){
  return(c(mean(x),quantile(x,c(0.025,0.5,0.975))))
})
