# library(demography)
# library(fda)
# library(grDevices)
# 
# ## Retrieve data ##
# nether.mort2 <- hmd.mx("NLD", "zhongjingyap@gmail.com", "Zhongjing@2", "Netherlands")
# nether.mort <- extract.years(nether.mort2, 1899:2015) 
# nether.mort <- extract.ages(nether.mort, 0:99, combine.upper = TRUE)
# sm.nether.mort <- smooth.demogdata(nether.mort)

hz <- 10
M <- matrix(0, nrow = hz, ncol = 6)
M2 <- matrix(0, nrow = hz, ncol = 6)
a <- min(sm.nether.mort[["year"]])
b <- max(sm.nether.mort[["year"]])
cc <- 0
d <- max(sm.nether.mort[["age"]])
m <- 1995


for (yr in m:(b - 1)) {
  print(yr)
  
  test.mort <- extract.years(nether.mort, a:yr)
  sm.test.mort <- extract.years(sm.nether.mort, a:yr)
  h <- min(hz, b - yr)
  
  sig.fit <- sigfdm(sm.test.mort, series = "total", m = 2)
  sig.fcast <- sigforecast4(sig.fit, h)
  
  rsig.fit <- rsigfdm(sm.test.mort, series = "total", k = 13, activation = function(x) 1/(3*sqrt(13)))
  rsig.fcast <- sigforecast4(rsig.fit, h)
  
  rsig.fit2 <- rsigfdm(sm.test.mort, series = "total", k = 25, activation = function(x) 1/(3*sqrt(25)))#function(x) sigmoid::sigmoid(x))
  rsig.fcast2 <- sigforecast4(rsig.fit2, h)
  
  rsig.fit3 <- rsigfdm(sm.test.mort, series = "total", k = 50, activation = function(x) 1/(3*sqrt(50)))#function(x) sigmoid::relu(x))
  rsig.fcast3 <- sigforecast4(rsig.fit3, h)
  
  rsig.fit4 <- rsigfdm(sm.test.mort, series = "total", k = 75, activation = function(x) 1/(3*sqrt(75)))#function(x) sigmoid::leakyrelu(x))
  rsig.fcast4 <- sigforecast4(rsig.fit4, h)
  
  rsig.fit5 <- rsigfdm(sm.test.mort, series = "total", k = 100, activation = function(x) 1/(3*sqrt(100)))#function(x) sigmoid::SoftMax(x))
  rsig.fcast5 <- sigforecast4(rsig.fit5, h)
  
  mse <- matrix(0, nrow = hz, ncol = 6)
  mape <- matrix(0, nrow = hz, ncol = 6)
  
  for (age in cc:d) {
    actual <- matrix(log(nether.mort[["rate"]][["total"]])[age + 1, -(1:(yr - a + 1))])
    
    # Calculate forecasts
    sig_fc <- sig.fcast$forecast[age + 1, ]
    rsig_fc <- rsig.fcast$forecast[age + 1, ]
    rsig_fc2 <- rsig.fcast2$forecast[age + 1, ]
    rsig_fc3 <- rsig.fcast3$forecast[age + 1, ]
    rsig_fc4 <- rsig.fcast4$forecast[age + 1, ]
    rsig_fc5 <- rsig.fcast5$forecast[age + 1, ]
    
    data <- cbind(actual[1:length(sig_fc)], sig_fc,rsig_fc,rsig_fc2,rsig_fc3,rsig_fc4,rsig_fc5)
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
      se <- rbind(se, c(0, 0, 0, 0, 0,0))
      ape <- rbind(ape, c(0, 0, 0, 0, 0,0))
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
colnames(results) <- colnames(results2) <- c("HUts", "k=13","k=25","k=50","k=75","k=100")
rownames(results) <- rownames(results2) <- c("h=1",2,3,4,5,6,7,8,9,10)
View(results)
