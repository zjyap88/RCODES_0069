# library(demography)
# library(fda)
# 
# ##Retreive data## 
# france.mort2 <- hmd.mx("FRATNP", "zhongjingyap@gmail.com", "Zhongjing@2", "France")
# france.mort <- extract.years(france.mort2, 1899:2015) 
# france.mort <- extract.ages(france.mort, 0:100, combine.upper = TRUE)
# sm.france.mort <- smooth.demogdata(france.mort)

hz <- 10
a <- min(sm.france.mort[["year"]])
b <- max(sm.france.mort[["year"]])
cc <- 0
d <- max(sm.france.mort[["age"]])
m <- 1995
M1 <- M2 <- M3 <- M4 <- M5 <- matrix(0, nrow=d+1, ncol = hz)
M6 <- M7 <- M8 <- M9 <- M10 <- matrix(0, nrow=d+1, ncol = hz)
M11 <- M12 <- M13 <- M14 <- M15 <- matrix(0, nrow=d+1, ncol = hz)

for(yr in m:(b-1)){
  print(yr)
  
  test.mort <- extract.years(france.mort, a:yr)
  sm.test.mort <- extract.years(sm.france.mort, a:yr)
  h <- min(hz, b - yr)
  
  sig.fit <- sigfdm(sm.test.mort, series = "total", m = 2)
  sig.fcast <- sigforecast2(sig.fit, h, PI = TRUE)
  
  fdm.fit <- fdm(sm.test.mort, series = "total", method = "classical")
  fdm.fcast <- forecast(fdm.fit, h)
  
  fdm.fit.rob <- fdm(sm.test.mort, series = "total", method = "rapca", )
  fdm.fcast.rob <- forecast(fdm.fit.rob, h)
  
  fdm.fit.w <- fdm(sm.test.mort, series = "total", method = "classical", weight= TRUE, beta=0.1)
  fdm.fcast.w <- forecast(fdm.fit.w, h)
  
  lca_fit <- lca(test.mort, series = "total", max.age = d)
  lca_fcast <- forecast(lca_fit, h)
  

  actual <- log(france.mort[["rate"]][["total"]])[,-(1:(yr-a+1))]
  sig_fc <- sig.fcast$forecast
  fdm_fc <- log(fdm.fcast$rate$total)
  fdm_fc_rob <- log(fdm.fcast.rob$rate$total)
  fdm_fc_w <- log(fdm.fcast.w$rate$total)
  lca_fc <- log(lca_fcast$rate$total)
  
  
  if(is.null((ncol(actual)))){
    pp <-1
  }
  else{
    pp<-min(10,ncol(actual))
    actual <- actual[,1:pp]
  }  
  if(dim(sig_fc)[2]<10){
    zero_matrix <- matrix(0,nrow=dim(sig_fc)[1], ncol=10-dim(sig_fc)[2])
    sig_fc <- cbind(sig_fc,zero_matrix)
    fdm_fc <- cbind(fdm_fc,zero_matrix)
    fdm_fc_rob <- cbind(fdm_fc_rob,zero_matrix)
    fdm_fc_w <- cbind(fdm_fc_w,zero_matrix)
    lca_fc <- cbind(lca_fc,zero_matrix)
    actual <- cbind(actual,zero_matrix)
  }
  M1 <- M1 + (actual-sig_fc)^2
  M2 <- M2 + (actual-fdm_fc)^2
  M3 <- M3 + (actual-fdm_fc_rob)^2
  M4 <- M4 + (actual-fdm_fc_w)^2
  M5 <- M5 + (actual-lca_fc)^2
  
  M6 <- M6 + abs(actual-sig_fc)
  M7 <- M7 + abs(actual-fdm_fc)
  M8 <- M8 + abs(actual-fdm_fc_rob)
  M9 <- M9 + abs(actual-fdm_fc_w)
  M10 <- M10 + abs(actual-lca_fc)
  
  M11 <- M11 + actual-sig_fc
  M12 <- M12 + actual-fdm_fc
  M13 <- M13 + actual-fdm_fc_rob
  M14 <- M14 + actual-fdm_fc_w
  M15 <- M15 + actual-lca_fc
  
}

#Take average MSE
k <- 20
for(n in 1:10){
  M1[,n] <- M1[,n]/k
  M2[,n] <- M2[,n]/k
  M3[,n] <- M3[,n]/k
  M4[,n] <- M4[,n]/k
  M5[,n] <- M5[,n]/k
  M6[,n] <- M6[,n]/k
  M7[,n] <- M7[,n]/k
  M8[,n] <- M8[,n]/k
  M9[,n] <- M9[,n]/k
  M10[,n] <- M10[,n]/k
  M11[,n] <- M11[,n]/k
  M12[,n] <- M12[,n]/k
  M13[,n] <- M13[,n]/k
  M14[,n] <- M14[,n]/k
  M15[,n] <- M15[,n]/k
  k <- k-1
}
colnames(M1) <- colnames(M2) <-colnames(M3) <-colnames(M4) <-colnames(M5) <-colnames(M6) <-colnames(M7) <-colnames(M8) <-colnames(M9) <-colnames(M10) <-colnames(M11) <-colnames(M12) <-colnames(M13) <-colnames(M14) <-colnames(M15) <-1:10
M_France <- list(MSE_SIG=M1,MSE_HU=M2,MSE_HUrob=M3,MSE_wHU=M4,MSE_LC=M5,
                 MAE_SIG=M6,MAE_HU=M7,MAE_HUrob=M8,MAE_wHU=M9,MAE_LC=M10,
                 ME_SIG=M11,ME_HU=M12,ME_HUrob=M13,ME_wHU=M14,ME_LC=M15)


