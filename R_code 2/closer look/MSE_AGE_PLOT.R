lwd<-3
label_size <- 1.3    
title_size <- 1.7    
axis_size  <- 1.2    
legend_size <- 1.3

for(K in c(1,5,10)){
  layout(matrix(c(1, 3, 2, 3), ncol = 2, byrow = TRUE), widths = c(1, 0.2))
  par(oma = c(1, 1, 1, 1))
  
  par(mar = c(4, 4, 2, 1))
  plot(M_France[[2]][,K], type = "l", ylab = "MSE", xlab = "",
       main = "(a)", col = "blue", lty = 2, lwd = lwd,
       ylim = c(min(M_France[[1]][,K],M_France[[2]][,K],M_France[[3]][,K],
                    M_France[[4]][,K]), max(M_France[[1]][,K],M_France[[2]][,K],
                                            M_France[[3]][,K],M_France[[4]][,K])),
       cex.lab = label_size,     
       cex.main = title_size,    
       cex.axis = axis_size)
  lines(M_France[[1]][,K], col = "red", lty = 1, lwd = lwd)
  lines(M_France[[3]][,K], col = "darkgreen", lty = 3, lwd = lwd)
  lines(M_France[[4]][,K], col = "orange", lty = 4, lwd = lwd)
  
  par(mar = c(4, 4, 2, 1))
  plot(M_Australia[[2]][,K], type = "l", ylab = "MSE", xlab = "Age",
       main = "(b)", col = "blue", lty = 2, lwd = lwd,
       ylim = c(min(M_Australia[[1]][,K],M_Australia[[2]][,K],M_Australia[[3]][,K],
                    M_Australia[[4]][,K]), max(M_Australia[[1]][,K],M_Australia[[2]][,K],
                                               M_Australia[[3]][,K],M_Australia[[4]][,K])),
       cex.lab = label_size,     
       cex.main = title_size,    
       cex.axis = axis_size)
  lines(M_Australia[[1]][,K], col = "red", lty = 1, lwd = lwd)
  lines(M_Australia[[3]][,K], col = "darkgreen", lty = 3, lwd = lwd)
  lines(M_Australia[[4]][,K], col = "orange", lty = 4, lwd = lwd)
  

  par(mar = c(0, 0, 0, 0))
  plot.new()
  legend("center", legend = c("HUts", "wHU", "HUrob", "wHU"),
         col = c("red", "orange", "darkgreen", "blue"),
         lty = c(1, 4, 3, 2),
         lwd = lwd,
         bty = "n",
         cex = legend_size)
  
}

