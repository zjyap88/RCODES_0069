# library(demography)
# library(fda)
# library(grDevices)
# 
# ## Retrieve data ##
# belgium.mort2 <- hmd.mx("BEL", "zhongjingyap@gmail.com", "Zhongjing@2", "Belgium")
# belgium.mort <- extract.years(belgium.mort2, 1920:2015) 
# belgium.mort <- extract.ages(belgium.mort, 0:100, combine.upper = TRUE)
# sm.belgium.mort <- smooth.demogdata(belgium.mort)

hz <- 10
M <- matrix(0, nrow = hz, ncol = 6)
M2 <- matrix(0, nrow = hz, ncol = 6)
a <- min(sm.belgium.mort[["year"]])
b <- max(sm.belgium.mort[["year"]])
cc <- 0
d <- max(sm.belgium.mort[["age"]])
m <- 1995


for (yr in m:(b - 1)) {
  print(yr)
  
  test.mort <- extract.years(belgium.mort, a:yr)
  sm.test.mort <- extract.years(sm.belgium.mort, a:yr)
  h <- min(hz, b - yr)
  
  sig.fit <- sigfdm(sm.test.mort, series = "total", m = 2)
  sig.fcast <- sigforecast4(sig.fit, h)
  
  sig.fit2 <- sigfdm2(sm.test.mort, series = "total", m = 2)
  sig.fcast2 <- sigforecast4(sig.fit2, h)
  
  sig.fit3 <- sigfdm4(sm.test.mort, series = "total", m = 2, order=NULL, 
                      var.explained = 0.9)
  sig.fcast3 <- sigforecast4(sig.fit3, h)
  
  sig.fit4 <- sigfdm2(sm.test.mort, series = "total", m = 2, order = NULL, 
                      var.explained = 0.9)
  sig.fcast4 <- sigforecast4(sig.fit4, h)
  
  
  
  sig.fit5 <- sigfdm4(sm.test.mort, series = "total", m = 2, order=NULL,
                      var.explained = 0.9999)
  sig.fcast5 <- sigforecast4(sig.fit5, h)
  
  sig.fit6 <- sigfdm2(sm.test.mort, series = "total", m=2, order = NULL, 
                      var.explained = 0.9999)
  sig.fcast6 <- sigforecast4(sig.fit6, h)
  
  
  mse <- matrix(0, nrow = hz, ncol = 6)
  mape <- matrix(0, nrow = hz, ncol = 6)
  
  for (age in cc:d) {
    actual <- matrix(log(belgium.mort[["rate"]][["total"]])[age + 1, -(1:(yr - a + 1))])
    
    # Calculate forecasts
    sig_fc <- sig.fcast$forecast[age + 1, ]
    sig_fc2 <- sig.fcast2$forecast[age + 1, ]
    sig_fc3 <- sig.fcast3$forecast[age + 1, ]
    sig_fc4 <- sig.fcast4$forecast[age + 1, ]
    sig_fc5 <- sig.fcast5$forecast[age + 1, ]
    sig_fc6 <- sig.fcast6$forecast[age + 1, ]
    
    
    data <- cbind(actual[1:length(sig_fc)], sig_fc,sig_fc2,sig_fc3,sig_fc4,sig_fc5,
                  sig_fc6)
    data2 <- data
    
    # Compute squared errors
    for (i in 2:7) {
      for (k in 1:h) {
        data[k, i] <- (data[k, i] - data[k, 1])^2
        data2[k, i] <- abs(data2[k, i] - data2[k, 1])
      }
    }
    
    se <- data[, 2:7]
    ape <- data2[, 2:7]
    
    while (nrow(se) < hz || is.null(nrow(se))) {
      se <- rbind(se, c(0, 0, 0, 0,0,0))
      ape <- rbind(ape, c(0, 0, 0, 0,0,0))
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
colnames(results) <- colnames(results2) <- c("HUts","HUts2",
                                             "HUts_0.9", "HUts2_0.9",
                                             "HUts_0.9999", "HUts2_0.9999")
rownames(results) <- rownames(results2) <- c("h=1",2,3,4,5,6,7,8,9,10)
View(results)
# MSE_list_5[["Belgium"]] <- results
# MAE_list_5[["Belgium"]] <- results2

