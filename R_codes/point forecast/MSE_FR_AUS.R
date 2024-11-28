results <- MSE_list_5$France
results2 <- MSE_list_5$Australia
layout(matrix(c(1, 2, 3), ncol = 3), widths = c(1, 1, 0.5))
par(oma = c(1, 1, 1, 1))

par(mar = c(4, 4, 2, 1))
plot(results[, 2], type = "l", ylab = "MSE", xlab = "Forecast Horizon",
     main = "(a)", col = "blue", lty = 2, lwd = 2,
     ylim = c(min(results[, 1]), max(results[, 2])))
lines(results[, 1], col = "red", lty = 1, lwd = 1.5)
lines(results[, 3], col = "darkgreen", lty = 3, lwd = 3)
lines(results[, 4], col = "orange", lty = 4, lwd = 2)

par(mar = c(4, 4, 2, 1))
plot(results2[, 2], type = "l", ylab = "MSE", xlab = "Forecast Horizon",
     main = "(b)", col = "blue", lty = 2, lwd = 2,
     ylim = c(min(results2[, 1]), max(results2[, 2])))
lines(results2[, 1], col = "red", lty = 1, lwd = 1.5)
lines(results2[, 3], col = "darkgreen", lty = 3, lwd = 3)
lines(results2[, 4], col = "orange", lty = 4, lwd = 2)

par(mar = c(0, 0, 0, 0))
plot.new()

legend("center", legend = c("HUts", "wHU", "HUrob", "HU"),
       col = c("red", "orange", "darkgreen", "blue"),
       lty = c(1, 4, 3, 2),
       lwd = c(1.5, 2, 3, 2),
       bty = "n",
       cex = 0.9)
