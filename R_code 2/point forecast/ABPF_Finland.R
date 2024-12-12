# library(demography)
# library(fda)
# library(grDevices)
# 
# ## Retrieve data ##
# finland.mort2 <- hmd.mx("FIN", "zhongjingyap@gmail.com", "Zhongjing@2", "Finland")
# finland.mort <- extract.years(finland.mort2, 1899:2015) 
# finland.mort <- extract.ages(finland.mort, 0:96, combine.upper = TRUE)
# 
# finland.mort$rate$total <- finland.mort$rate$total + 
#   min(finland.mort$rate$total[finland.mort$rate$total > 0])
# sm.finland.mort <- smooth.demogdata(finland.mort)

hz <- 10
M <- matrix(0, nrow = hz, ncol = 4)
M2 <- matrix(0, nrow = hz, ncol = 4)
a <- min(sm.finland.mort[["year"]])
b <- max(sm.finland.mort[["year"]])
cc <- 0
d <- max(sm.finland.mort[["age"]])
m <- 1995


for (yr in m:(b - 1)) {
  print(yr)
  
  test.mort <- extract.years(finland.mort, a:yr)
  sm.test.mort <- extract.years(sm.finland.mort, a:yr)
  h <- min(hz, b - yr)
  
  sig.fit <- sigfdm(sm.test.mort, series = "total", m = 2)
  sig.fcast <- sigforecast4(sig.fit, h)
  
  fdm.fit <- fdm(sm.test.mort, series = "total", method = "classical")
  fdm.fcast <- forecast(fdm.fit, h)
  
  fdm.fit.rob <- fdm(sm.test.mort, series = "total", method = "rapca", )
  fdm.fcast.rob <- forecast(fdm.fit.rob, h)
  
  fdm.fit.w <- fdm(sm.test.mort, series = "total", method = "classical", weight= TRUE, beta=0.1)
  fdm.fcast.w <- forecast(fdm.fit.w, h)
  
  mse <- matrix(0, nrow = hz, ncol = 4)
  mape <- matrix(0, nrow = hz, ncol = 4)
  
  for (age in cc:d) {
    actual <- matrix(log(finland.mort[["rate"]][["total"]])[age + 1, -(1:(yr - a + 1))])
    
    # Calculate forecasts
    sig_fc <- sig.fcast$forecast[age + 1, ]
    fdm_fc <- fdm.fcast$rate$total[age + 1, ]
    fdm_fc_rob <- fdm.fcast.rob$rate$total[age + 1, ]
    fdm_fc_w <- fdm.fcast.w$rate$total[age + 1, ]
    
    data <- cbind(actual[1:length(sig_fc)], sig_fc, log(fdm_fc),log(fdm_fc_rob),
                  log(fdm_fc_w))
    data2 <- data
    
    # Compute squared errors
    for (i in 2:5) {
      for (k in 1:h) {
        data[k, i] <- (data[k, i] - data[k, 1])^2
        data2[k, i] <- abs(data2[k, i] - data2[k, 1])
      }
    }
    
    se <- data[, 2:5]
    ape <- data2[, 2:5]
    
    while (nrow(se) < hz || is.null(nrow(se))) {
      se <- rbind(se, c(0, 0, 0, 0))
      ape <- rbind(ape, c(0, 0, 0, 0))
    }
    
    mse <- mse + se
    mape <- mape + ape
  }
  
  mse <- mse / (d - cc + 1) # divides age
  M <- M + mse
  mape <- mape / (d - cc + 1) # divides age
  M2 <- M2 + mape
}

# Take average MSE
for (n in 1:hz) {
  M[n, ] <- M[n, ] / (b - m + 1 - n) # divides number of years
  M2[n, ] <- M2[n, ] / (b - m + 1 - n)
}

results <- M
results2 <- M2
colnames(results) <- colnames(results2) <- c("HUts", "HU","HUrob","wHU")
rownames(results) <- rownames(results2) <- c("h=1",2,3,4,5,6,7,8,9,10)
# MSE_list_5[["Finland"]] <- results
# MAE_list_5[["Finland"]] <- results2

