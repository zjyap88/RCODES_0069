# library(demography)
# library(fda)
# library(grDevices)
# 
# ## Retrieve data ##
# bulgaria.mort2 <- hmd.mx("BGR", "zhongjingyap@gmail.com", "Zhongjing@2", "Bulgaria")
# bulgaria.mort <- extract.years(bulgaria.mort2, 1947:2015) 
# bulgaria.mort <- extract.ages(bulgaria.mort, 0:100, combine.upper = TRUE)
# sm.bulgaria.mort <- smooth.demogdata(bulgaria.mort)

hz <- 10
a <- min(sm.bulgaria.mort[["year"]])
b <- max(sm.bulgaria.mort[["year"]])
cc <- 0
d <- max(sm.bulgaria.mort[["age"]])
m <- 1995
count <- count_sigPI <- count_sigPI_adj <- count_wHUPI <- count_wHUPI_adj<- 0

for (yr in m:(b - 1)) {
  print(yr)
  test.mort <- extract.years(bulgaria.mort, a:yr)
  sm.test.mort <- extract.years(sm.bulgaria.mort, a:yr)
  h <- min(hz, b - yr)
  
  sig.fit <- sigfdm(sm.test.mort, series = "total", m = 2)
  sig.fcast <- sigforecast4(sig.fit, h)
  pred_interval1 <- PI_sigfc2(sig.fcast, seed = 1234, nsim = 1000, adj_interval = FALSE)
  pred_interval2 <- PI_sigfc2(sig.fcast, seed = 1234, nsim = 1000, adj_interval = TRUE)
  
  fdm.fit.w <- fdm(sm.test.mort, series = "total", method = "classical", weight= TRUE, beta=0.1)
  fdm.fcast.w <- forecast(fdm.fit.w, h)
  pred_interval3 <- PI_fdm(fdm.fcast.w, nsim=1000, seed=1234, epsilon=sig.fit$epsilon, adj_interval=FALSE)
  pred_interval4 <- PI_fdm(fdm.fcast.w, nsim=1000, seed=1234, epsilon=sig.fit$epsilon, adj_interval=TRUE)
  
  for (age in cc:d) {
    ind <- sig.fcast$fc_year[1]-a+1
    actual <- matrix(log(bulgaria.mort[["rate"]][["total"]])[age + 1, ind:(ind+h-1)])
    
    ecp  <- cbind(pred_interval1$lower[age + 1, ], actual, pred_interval1$upper[age + 1, ], 0)
    ecp2 <- cbind(pred_interval2$lower[age + 1, ], actual, pred_interval2$upper[age + 1, ], 0)
    ecp3 <- cbind(pred_interval3$lower[age + 1, ], actual, pred_interval3$upper[age + 1, ], 0)
    ecp4 <- cbind(pred_interval4$lower[age + 1, ], actual, pred_interval4$upper[age + 1, ], 0)
    
    count <- count + nrow(ecp)
    count_sigPI <- count_sigPI + ecp_count(ecp)
    count_sigPI_adj <- count_sigPI_adj + ecp_count(ecp2)
    count_wHUPI <- count_wHUPI + ecp_count(ecp3)
    count_wHUPI_adj <- count_wHUPI_adj + ecp_count(ecp4)
    
  }
}

boot_count_sigPI <- round(count_sigPI / count, 6)
boot_count_sigPI_adj <- round(count_sigPI_adj / count, 6)
boot_count_wHUPI <- round(count_wHUPI / count, 6)
boot_count_wHUPI_adj <- round(count_wHUPI_adj / count, 6)


cat("\nHUts empirical coverage without adjustment:", boot_count_sigPI, "\n")
cat("\nHUts empirical coverage with adjustment:", boot_count_sigPI_adj, "\n")
cat("\nwHU empirical coverage without adjustment:", boot_count_wHUPI, "\n")
cat("\nwHU empirical coverage with adjustment:", boot_count_wHUPI_adj, "\n")

