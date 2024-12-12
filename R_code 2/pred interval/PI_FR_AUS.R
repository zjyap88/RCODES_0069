lwd<-2.5
label_size <- 1.3    
title_size <- 1.7    
axis_size  <- 1.2    
legend_size <- 1.4   

layout(matrix(c(1, 2, 3), ncol = 3), widths = c(1, 1, 0.6))
par(oma = c(1, 1, 1, 1))
par(mar = c(4, 4, 2, 1))
plot(extract.years(france.mort, 2015), series = "total", type = "p", pch=16, cex=1,
     main = "(a)",
     cex.lab = label_size,     
     cex.main = title_size,    
     cex.axis = axis_size, ylim=c(-10.5,0))
lines(france.mort$age, FRpred_interval1$lower, col = "red", lty = 1, lwd = lwd)
lines(france.mort$age, FRpred_interval1$upper, col = "red", lty = 1, lwd = lwd)
lines(france.mort$age, FRpred_interval3$lower, col = "orange", lty = 4, lwd = lwd)
lines(france.mort$age, FRpred_interval3$upper, col = "orange", lty = 4, lwd = lwd)
points(france.mort$age, log(extract.years(france.mort, 2015)$rate$total))

par(mar = c(4, 4, 2, 1))
plot(extract.years(france.mort, 2015), series = "total", type = "p", pch=16, cex=1,
     main = "(b)",
     cex.lab = label_size,     
     cex.main = title_size,    
     cex.axis = axis_size, ylim=c(-10.5,0))
lines(france.mort$age, FRpred_interval2$lower, col = "red", lty = 1, lwd = lwd)
lines(france.mort$age, FRpred_interval2$upper, col = "red", lty = 1, lwd = lwd)
lines(france.mort$age, FRpred_interval4$lower, col = "orange", lty = 4, lwd = lwd)
lines(france.mort$age, FRpred_interval4$upper, col = "orange", lty = 4, lwd = lwd)
points(france.mort$age, log(extract.years(france.mort, 2015)$rate$total))

par(mar = c(0, 0, 0, 0))
plot.new()

legend("center", legend = c("HUts", "wHU", "Observed"),
       col = c("red", "orange", "black"),
       lty = c(1, 4, NA),
       pch =c(NA,NA,16),
       lwd = lwd,
       bty = "n",
       cex = legend_size)


layout(matrix(c(1, 2, 3), ncol = 3), widths = c(1, 1, 0.6))
par(oma = c(1, 1, 1, 1))
par(mar = c(4, 4, 2, 1))
plot(extract.years(aus.mort, 2015), series = "total", type = "p", pch=16, cex=1,
     main = "(a)",
     cex.lab = label_size,     
     cex.main = title_size,    
     cex.axis = axis_size)
lines(aus.mort$age, AUSpred_interval1$lower, col = "red", lty = 1, lwd = lwd)
lines(aus.mort$age, AUSpred_interval1$upper, col = "red", lty = 1, lwd = lwd)
lines(aus.mort$age, AUSpred_interval3$lower, col = "orange", lty = 4, lwd = lwd)
lines(aus.mort$age, AUSpred_interval3$upper, col = "orange", lty = 4, lwd = lwd)
points(aus.mort$age, log(extract.years(aus.mort, 2015)$rate$total))

par(mar = c(4, 4, 2, 1))
plot(extract.years(aus.mort, 2015), series = "total", type = "p", pch=16, cex=1,
     main = "(b)",
     cex.lab = label_size,     
     cex.main = title_size,    
     cex.axis = axis_size)
lines(aus.mort$age, AUSpred_interval2$lower, col = "red", lty = 1, lwd = lwd)
lines(aus.mort$age, AUSpred_interval2$upper, col = "red", lty = 1, lwd = lwd)
lines(aus.mort$age, AUSpred_interval4$lower, col = "orange", lty = 4, lwd = lwd)
lines(aus.mort$age, AUSpred_interval4$upper, col = "orange", lty = 4, lwd = lwd)
points(aus.mort$age, log(extract.years(aus.mort, 2015)$rate$total))

par(mar = c(0, 0, 0, 0))
plot.new()

legend("center", legend = c("HUts", "wHU", "Observed"),
       col = c("red", "orange", "black"),
       lty = c(1, 4, NA),
       pch =c(NA,NA,16),
       lwd = lwd,
       bty = "n",
       cex = legend_size)
