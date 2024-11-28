for(K in c(1,5,10)){
  layout(matrix(c(1, 3, 2, 3), ncol = 2, byrow = TRUE), widths = c(1, 0.15))
  par(oma = c(1, 1, 1, 1))
  
  par(mar = c(4, 4, 2, 1))
  plot(M_France[[2]][,K], type = "l", ylab = "MSE", xlab = "",
       main = "(a)", col = "blue", lty = 2, lwd = 2,
       ylim = c(min(M_France[[1]][,K],M_France[[2]][,K],M_France[[3]][,K],
                    M_France[[4]][,K]), max(M_France[[1]][,K],M_France[[2]][,K],
                                            M_France[[3]][,K],M_France[[4]][,K])))
  lines(M_France[[1]][,K], col = "red", lty = 1, lwd = 1.5)
  lines(M_France[[3]][,K], col = "darkgreen", lty = 3, lwd = 3)
  lines(M_France[[4]][,K], col = "orange", lty = 4, lwd = 2)
  
  par(mar = c(4, 4, 2, 1))
  plot(M_Australia[[2]][,K], type = "l", ylab = "MSE", xlab = "Age",
       main = "(b)", col = "blue", lty = 2, lwd = 2,
       ylim = c(min(M_Australia[[1]][,K],M_Australia[[2]][,K],M_Australia[[3]][,K],
                    M_Australia[[4]][,K]), max(M_Australia[[1]][,K],M_Australia[[2]][,K],
                                               M_Australia[[3]][,K],M_Australia[[4]][,K])))
  lines(M_Australia[[1]][,K], col = "red", lty = 1, lwd = 1.5)
  lines(M_Australia[[3]][,K], col = "darkgreen", lty = 3, lwd = 3)
  lines(M_Australia[[4]][,K], col = "orange", lty = 4, lwd = 2)
  

  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", legend = c("HUts", "wHU", "HUrob", "wHU"),
         col = c("red", "orange", "darkgreen", "blue"),
         lty = c(1, 4, 3, 2),
         lwd = c(1.5, 2, 3, 2),
         bty = "n",
         cex = 0.8)
  
}

