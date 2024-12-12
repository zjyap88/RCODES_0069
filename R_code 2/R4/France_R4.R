# library(demography)
# library(fda)
# library(grDevices)
# 
# ## Retrieve data ##
# france.mort2 <- hmd.mx("FRATNP", "zhongjingyap@gmail.com", "Zhongjing@2", "France")
# france.mort <- extract.years(france.mort2, 1899:2015)
# france.mort <- extract.ages(france.mort, 0:100, combine.upper = TRUE)
# sm.france.mort <- smooth.demogdata(france.mort)

hz <- 10
M <- matrix(0, nrow = hz, ncol = 2)
M2 <- matrix(0, nrow = hz, ncol = 2)
a <- min(sm.france.mort[["year"]])
b <- max(sm.france.mort[["year"]])
cc <- 0
d <- max(sm.france.mort[["age"]])
m <- 1995


for (yr in m:(b - 1)) {
  print(yr)
  
  test.mort <- extract.years(france.mort, a:yr)
  sm.test.mort <- extract.years(sm.france.mort, a:yr)
  h <- min(hz, b - yr)
  
  sig.fit <- sigfdm(sm.test.mort, series = "total", m = 2)
  sig.fcast <- sigforecast4(sig.fit, h)
  
  sig.fit2 <- sigfdmNS(test.mort, series = "total", m = 2)
  sig.fcast2 <- sigforecastNS(sig.fit2, h)
  
  mse <- matrix(0, nrow = hz, ncol = 2)
  mape <- matrix(0, nrow = hz, ncol = 2)
  
  for (age in cc:d) {
    actual <- matrix(log(france.mort[["rate"]][["total"]])[age + 1, -(1:(yr - a + 1))])
    
    # Calculate forecasts
    sig_fc <- sig.fcast$forecast[age + 1, ]
    sig_fc2 <- sig.fcast2$forecast[age + 1, ]
    
    
    data <- cbind(actual[1:length(sig_fc)], sig_fc,sig_fc2)
    data2 <- data
    
    # Compute squared errors
    for (i in 2:3) {
      for (k in 1:h) {
        data[k, i] <- (data[k, i] - data[k, 1])^2
        data2[k, i] <- abs(data2[k, i] - data2[k, 1])
      }
    }
    
    se <- data[, 2:3]
    ape <- data2[, 2:3]
    
    while (nrow(se) < hz || is.null(nrow(se))) {
      se <- rbind(se, c(0, 0))
      ape <- rbind(ape, c(0, 0))
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
colnames(results) <- colnames(results2) <- c("HUts","HUts_NS")
rownames(results) <- rownames(results2) <- c("h=1",2,3,4,5,6,7,8,9,10)
# MSE_list_5[["France"]] <- results
# MAE_list_5[["France"]] <- results2

