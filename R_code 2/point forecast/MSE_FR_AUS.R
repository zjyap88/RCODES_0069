results <- MSE_list_5$France
results2 <- MSE_list_5$Australia

lwd<-4
label_size <- 1.3    
title_size <- 1.7    
axis_size  <- 1.2    
legend_size <- 1.3   

layout(matrix(c(1, 2, 3), ncol = 3), widths = c(1, 1, 0.5))
par(oma = c(1, 1, 1, 1))
par(mar = c(4, 4, 2, 1))
plot(results[, 2], type = "l", ylab = "MSE", xlab = "Forecast Horizon",
     main = "(a)", col = "blue", lty = 2, lwd = lwd,
     ylim = c(min(results[, 1]), max(results[, 2])),
     cex.lab = label_size,     
     cex.main = title_size,    
     cex.axis = axis_size)
lines(results[, 1], col = "red", lty = 1, lwd = lwd)
lines(results[, 3], col = "darkgreen", lty = 3, lwd = lwd)
lines(results[, 4], col = "orange", lty = 4, lwd = lwd)

par(mar = c(4, 4, 2, 1))
plot(results2[, 2], type = "l", ylab = "MSE", xlab = "Forecast Horizon",
     main = "(b)", col = "blue", lty = 2, lwd = lwd,
     ylim = c(min(results2[, 1]), max(results2[, 2])),
     cex.lab = label_size,     
     cex.main = title_size,    
     cex.axis = axis_size)
lines(results2[, 1], col = "red", lty = 1, lwd = lwd)
lines(results2[, 3], col = "darkgreen", lty = 3, lwd = lwd)
lines(results2[, 4], col = "orange", lty = 4, lwd = lwd)

par(mar = c(0, 0, 0, 0))
plot.new()

legend("center", legend = c("HUts", "wHU", "HUrob", "HU"),
       col = c("red", "orange", "darkgreen", "blue"),
       lty = c(1, 4, 3, 2),
       lwd = lwd,
       bty = "n",
       cex = legend_size)
