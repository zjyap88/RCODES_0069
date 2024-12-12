library(demography)
library(fda)
library(grDevices)
load("~/fmr.RData")

sigfdm <- function(data, series = names(data$rate)[3], order = 6, ages = data$age, 
                   max.age = max(ages), lambda = 3, mean = TRUE, level = FALSE, transform = TRUE,
                   m = 3){
  
  # Check if the series is in the data
  if(!is.element(toupper(series), toupper(names(data$rate)))) {
    stop(paste("Series", series, "not found"))
  }
  
  # Get the series from the data
  i <- match(toupper(series), toupper(names(data$rate)))
  series_data <- as.matrix(data$rate[[i]])
  
  # Transform the mortality rate series using BoxCox transformation
  mx <- BoxCox(series_data, data$lambda)
  mx[mx < -1e+09] <- NA
  
  # Create functional time series object
  data.fts <- fts(ages, mx, start = data$year[1], xname = "Age", yname = "Mortality rate")
  ytsp <- data.fts$time
  char_value = colnames(data.fts$y)
  year <- as.numeric(substring(char_value, 1, 4))
  colnames(data.fts$y) = year
  
  ngrid <- max(500, ncol(data.fts$y))
  n <- ncol(data.fts$y)
  mm <- length(data.fts$x)
  x <- 1:mm
  yy <- matrix(NA, nrow = ngrid, ncol = n)
  
  # Apply spline interpolation to the data
  for(i in 1:n) {
    yy[,i] <- spline(x, data.fts$y[,i], n = ngrid)$y
  }
  xx <- seq(min(x), max(x), l = ngrid)
  
  # Calculate the mean and standard error of the mortality rates
  ax <- rowMeans(yy, na.rm = TRUE)
  axse <- approx(xx, sqrt(apply(yy, 1, var) / n), xout = x)$y
  
  # Initialize basis and coefficient matrices
  coeff <- basis <- NULL
  coeff <- cbind(rep(1, n), coeff)
  basis <- cbind(approx(xx, ax, xout = x)$y, basis)
  colnames(coeff)[1] <- colnames(basis)[1] <- "mean"
  
  # Mean-adjusted mortality rates
  Y <- sweep(yy, 1, ax)
  
  # Calculate signature of paths for dimensionality reduction
  d <- 3
  sig <- matrix(0, nrow = nrow(yy), ncol = ((d^(m + 1) - 1) / (d - 1)) - 1)
  for(i in 1:nrow(yy)) {
    sig[i,] <- tsig(embed_path(Y[i,]), m)
  }
  
  # Principal Component Analysis on the signature
  sigphi <- prcomp.sig(sig)[["x"]][,1:order]
  beta <- matrix(0, nrow = n, ncol = order)
  
  # Linear modeling of the mean-adjusted rates
  for(i in 1:n) {
    beta[i,] <- lm(Y[,i] ~ sigphi - 1)$coefficients
  }
  sigphi2 <- matrix(NA, nrow = length(x), ncol = order)
  for(i in 1:order) {
    sigphi2[,i] <- approx(xx, sigphi[,i], xout = x)$y
  }
  basis <- cbind(basis, sigphi2)
  coeff <- cbind(coeff, beta)
  colnames(basis)[2:(order + 1)] <- paste("Z", 1:order, sep = "")
  colnames(coeff)[2:(order + 1)] <- paste("beta", 1:order, sep = "")
  
  # Calculate fitted values
  fitted <- basis %*% t(coeff)
  colnames(fitted) <- colnames(mx)
  rownames(fitted) <- rownames(mx)
  
  # Create functional time series for the fitted values
  fits <- fts(data.fts$x, fitted, start = ytsp[1], frequency = 1, xname = data.fts$xname,
              yname = paste("Fitted", data.fts$yname))
  res <- fts(data.fts$x, data.fts$y - fitted)
  
  ov <- data[["obs.var"]][[series]]
  value <- switch(series,"male" = 1,"female" = 2,"total" = 3)
  s_ep <- data[["err"]][[value]][,1:dim(ov)[2]]
  
  epsil <- s_ep/sqrt(ov)
  return(list(sig = sig, basis = basis, coef = coeff, fitted = fits, year = data$year, 
              ages = ages, Y = Y, mean.se = axse, residuals = res, y = data.fts, obs.var = ov,
              pop = data$pop$total, epsilon = epsil))
}

sigforecast4 <- function(object, h, level = c(80, 95), adjust = TRUE) {
  nb <- ncol(object$basis)
  order <- nb - 1
  l <- nrow(object$coef)
  
  fc_beta <- varfcast <- matrix(NA, nrow = h, ncol = ncol(object$coef))
  qconf <- qnorm(0.5 + level / 200)
  
  fitted <- matrix(1, nrow = l, ncol = ncol(object$coef))
  fmodels <- list()
  
  # Forecast ARIMA models for each coefficient
  for (i in 2:ncol(object$coef)) {
    barima <- auto.arima(ts(object$coef[,i]))
    #barima <- arima(ts(object$coef[,i]), order=c(0,1,0))
    fitted[,i] <- fitted(barima)
    pred <- forecast(barima, h = h, level = level)
    fc_beta[,i] <- pred$mean
    varfcast[,i] <- ((pred$upper[,1] - pred$lower[,1]) / (2 * qconf[1]))^2
    fmodels[[i]] <- pred
  }
  varfcast[,1] <- 0
  fc_beta[,1] <- 1
  
  # Generate forecasts
  fc <- object$basis %*% t(fc_beta)
  colnames(fc) <- (tail(object$year, 1) + 1):(tail(object$year, 1) + h)
  rownames(fc) <- rownames(object$fitted$y)
  
  ytsp <- tsp(object$fitted$time)
  error <- ts(object$coef - fitted, start = ytsp[1], frequency = ytsp[3])
  
  # One-step forecasts and errors
  ferror <- onestepfcast <- object$y
  onestepfcast$y <- object$basis %*% t(fitted)
  onestepfcast$yname <- "One step forecasts"
  colnames(onestepfcast$y) <- colnames(object$y$y)
  
  ferror$y <- object$y$y - onestepfcast$y
  ferror$yname <- "One step errors"
  
  fmean <- fts(object$y$x, fc, start = ytsp[2], frequency = ytsp[3], xname = object$y$xname,
               yname = "Forecasts")
  
  res <- object$residuals
  res2 <- res$y^2
  vx <- rowMeans(res2, na.rm = TRUE)
  modelvar <- object$basis^2 %*% t(varfcast)
  totalvar <- sweep(modelvar, 1, vx + object$mean.se^2, "+")
  if (adjust & nb > 1) {
    adj.factor <- rowMeans(ferror$y^2, na.rm = TRUE)/totalvar[,1]
    totalvar <- sweep(totalvar, 1, adj.factor, "*")
  }
  else adj.factor <- 1
  s <- sqrt(abs(object$obs.var))
  ysd <- s * NA
  for (i in 1:ncol(ysd)) {
    if (sum(!is.na(s[,i])) >= 2) {
      ysd[,i] <- spline(object$ages, s[,i], n = nrow(totalvar))$y
    }
  }
  ysd <- rowMeans(ysd, na.rm = TRUE)
  observ <- ysd^2
  totalvar <- sweep(totalvar, 1, observ, "+")
  
  
  fse80 <- qnorm(0.5 + level[1] / 200) * sqrt(totalvar)
  fse95 <- qnorm(0.5 + level[2] / 200) * sqrt(totalvar)
  lower80 <- fmean$y - fse80
  upper80 <- fmean$y + fse80
  lower95 <- fmean$y - fse95
  upper95 <- fmean$y + fse95
  
  
  return(list(forecast = fmean$y, fc_beta = fc_beta, h = h, 
              fc_year = max(object$year)+(1:h)/stats::frequency(object$year),
              model_sigfdm = object, 
              lower = list(lower80 = lower80, lower95 = lower95), 
              upper = list(upper80 = upper80, upper95 = upper95), ferror = ferror,
              var = list(model = modelvar,error = vx, mean = object$mean.se^2, 
                         total = totalvar, coeff = varfcast, observ=observ,
                         adj.factor = adj.factor),
              coef_model = fmodels))
  
  
  
  
}

ecp_count <- function(object){
  n <- nrow(object)
  for(i in 1:n){
    if(object[i,1]<=object[i,2] & object[i,2]<=object[i,3]){
      object[i,4] <-1
    }
  }
  return(sum(object[,4]))
}

prcomp.sig <- function(x, retx = TRUE, center = TRUE, scale. = TRUE, tol = NULL, ...) {
  chkDots(...)
  x <- as.matrix(x)
  
  # Identify columns to scale
  cols_to_scale <- !(seq_len(ncol(x)) %in% c(1,4,13,40,85))
  
  # Apply scaling only to selected columns
  if (scale.) {
    x[, cols_to_scale] <- scale(x[, cols_to_scale], center = center, scale = scale.)
  } else if (center) {
    x[, cols_to_scale] <- scale(x[, cols_to_scale], center = center, scale = FALSE)
  }
  
  cen <- attr(x, "scaled:center")
  sc <- attr(x, "scaled:scale")
  if(any(sc == 0))
    stop("cannot rescale a constant/zero column to unit variance")
  
  s <- svd(x, nu = 0)
  s$d <- s$d / sqrt(max(1, nrow(x) - 1))
  
  if (!is.null(tol)) {
    rank <- sum(s$d > (s$d[1L]*tol))
    if (rank < ncol(x)) {
      s$v <- s$v[, 1L:rank, drop = FALSE]
      s$d <- s$d[1L:rank]
    }
  }
  
  dimnames(s$v) <- list(colnames(x), paste0("PC", seq_len(ncol(s$v))))
  r <- list(sdev = s$d, rotation = s$v, center = if(is.null(cen)) FALSE else cen, scale = if(is.null(sc)) FALSE else sc)
  
  if (retx) r$x <- x %*% s$v
  class(r) <- "prcomp"
  r
}

tsig <- function(path,m, flat=TRUE){
  n=dim(path)[2]
  d=dim(path)[1]
  diffs=path[,-1,drop=FALSE]-path[,-n,drop=FALSE]
  if (n<2){
    o=sapply(1:m,function(x)rep(0,d**x))
    if (flat) return (do.call(c,o))
    return (o)
  }
  r=lapply(1:(n-1),function(x)Reduce(kronecker,rep(list(diffs[,x]),m),accumulate=TRUE))
  facts=lapply(1:m,factorial)
  r=lapply(r,function(x)mapply("/",x,facts))
  chen=function(x,y) c(list(x[[1]]+y[[1]]),
                       lapply(2:m,
                              function(z)x[[z]]+y[[z]]
                              +Reduce("+",mapply(kronecker,x[1:z-1],rev(y[1:z-1]),
                                                 SIMPLIFY=FALSE))))
  o=Reduce(chen,r)
  if (flat) return (do.call(c,o))
  o
}

embed_path <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  n <- length(x)
  if (n <= 1) {
    stop("Input vector must have more than one element.")
  }
  path <- list()
  path[[length(path) + 1]] <- c(0, 0)
  path[[length(path) + 1]] <- c(x[1], x[1])
  for (i in 2:n) {
    path[[length(path) + 1]] <- c(x[i], x[i - 1])
    path[[length(path) + 1]] <- c(x[i], x[i])
  }
  third_coord <- seq(from = 0, to = 1, length.out = length(path))
  path <- mapply(function(p, tc) c(tc, p), path, third_coord, SIMPLIFY = FALSE)
  return(array(unlist(path), dim = c(3, length(path))))
}

PI_sigfc2 <- function(object, nsim = 1000, seed = 1234, bootstrap = TRUE, 
                      probs = c(0.025, 0.975), bc = FALSE, adj_interval = FALSE) {
  # Set seed
  if (!is.null(seed)){set.seed(seed)}
  
  n <- length(object$model_sigfdm$year)  # model refers to fdm fitted model
  p <- length(object$model_sigfdm$ages)
  h <- object$h
  nb <- length(object$coef_model)
  
  ridx <- 1:n
  # Set residuals to zero for the simulations
  resids <- object$model_sigfdm$residuals$y
  resids[is.na(resids)] <- 0
  fmean <- object$forecast
  sigma <- sqrt(object$var$observ)
  epsilon <- object$model_sigfdm$epsilon

    # Pre-simulate coefficients 
      sim_coeffs_list <- list()
      for (j in 2:nb) {
        mod <- object$coef_model[[j]]$model
        # Generate nsim simulations, each of length h
        sim_coeffs <- replicate(nsim, simulate(mod, nsim = h, bootstrap = bootstrap, future = TRUE), simplify = "matrix")
        if(h ==1) {sim_coeffs <- matrix(sim_coeffs, nrow=1)}
        sim_coeffs_list[[j]] <- sim_coeffs  # Dimensions: h x nsim
      }

  
  #Pre-sample residual indices 
    resids_indices <- replicate(nsim, sample(ridx, h, replace = TRUE), simplify = FALSE)
  
  #Sample epsilon
    epsilon_bootstrap <- array(0, c(p, h, nsim))

  #For each age, bootstrap epsilon independently
    for (aa in 1:p) {
      epsilon_hat_i <- epsilon[aa, ]
      epsilon_hat_i <- na.omit(epsilon_hat_i)

      epsilon_bootstrap[aa, , ] <- replicate(nsim, sample(epsilon_hat_i, h, replace = TRUE))
    }
  

  
  output <- array(0, c(p, h, nsim))
  
  for (i in 1:nsim) {
    output[,,i] <- object$model_sigfdm$basis[,1]  # extracts mean
    if (nb > 1) {
      for (j in 2:nb) {
        simulated_coeffs <- sim_coeffs_list[[j]][, i, drop=FALSE]
        output[,,i] <- output[,,i] + object$model_sigfdm$basis[,j] %*% matrix(simulated_coeffs, nrow = 1)
      }
    }
    resids_sample <- resids[, resids_indices[[i]]]
    output[,,i] <- output[,,i] + resids_sample  # e_t(x)
    # if (adjust.modelvar){
    #   output[,,i] <- fmean + sweep(output[,,i] - fmean, 1, sqrt(object$var$adj.factor), "*")
    # }
    output[,,i] <- output[,,i] + sigma * epsilon_bootstrap[,,i]
  }
  dimnames(output) <- list(object$model_sigfdm$ages, object$fc_year, 1:nsim)
  
  lower_quantile <- apply(output, c(1, 2), quantile, probs = probs[1], na.rm = TRUE)
  upper_quantile <- apply(output, c(1, 2), quantile, probs = probs[2], na.rm = TRUE)
  
  dimnames(lower_quantile) <- list(object$model_sigfdm$ages, object$fc_year)
  dimnames(upper_quantile) <- list(object$model_sigfdm$ages, object$fc_year)
  
  if(adj_interval){
    m <- 30
    ferr <- object$ferror$y[,-(1:m)]
    quantiles_matrix <- t(apply(ferr, 1, function(x) {
      quantile(x, probs = probs, na.rm = TRUE)
    }))
    dx <- quantiles_matrix[,2] - quantiles_matrix[,1]
    u_plus_l <- upper_quantile + lower_quantile
    u_minus_l <- upper_quantile - lower_quantile
    px <- dx / u_minus_l[,1]
    upl <- sweep(u_minus_l, 1, px, "*")
    
    lower_quantile <- 0.5*u_plus_l - upl
    upper_quantile <- 0.5*u_plus_l + upl
    
  }
  
  #if(!bc){
  return(list(lower = lower_quantile, upper = upper_quantile))
  #}
  # lowerbias_q <- upperbias_q <- matrix(NA, nrow = p, ncol = h)
  # 
  # for(i in 1:h){
  #   for(j in 1:p){
  #     z0 <- qnorm(sum(output[j,i,] < fmean[j,i]) / nsim)
  #     newprob1 <- pnorm(z0 + qnorm(probs[1]))
  #     newprob2 <- pnorm(z0 + qnorm(probs[2]))
  #     
  #     lowerbias_q[j,i] <- quantile(output[j,i,], probs = newprob1, na.rm = TRUE)
  #     upperbias_q[j,i] <- quantile(output[j,i,], probs = newprob2, na.rm = TRUE)
  #   }
  # }
  # return(list(lower = lower_quantile, upper = upper_quantile,
  #             lower_bc = lowerbias_q, upper_bc = upperbias_q))
}

PI_fdm <- function(object,nsim=1000,seed=1234,bootstrap=TRUE, epsilon, adj_interval=FALSE, probs = c(0.025, 0.975))
{
  if (!is.null(seed)){set.seed(seed)}
  
  n <- length(object$model$year)
  p <- length(object$age)
  h <- length(object$year)
  nb <- length(object$coeff)
  
  ridx <- (1:n)
  # Set residuals to zero for the simulations
  resids <- object$model$residuals$y
  resids[is.na(resids)] <- 0
  
  fmean <- BoxCox(object$rate[[1]],object$lambda)
  
  
  sigma <- sqrt(object$var$observ)
  #epsilon <- object$model_sigfdm$epsilon
  
  # Pre-simulate coefficients 
  sim_coeffs_list <- list()
  for (j in 2:nb) {
    mod <- object$coeff[[j]]$model$model
    # Generate nsim simulations, each of length h
    sim_coeffs <- replicate(nsim, simulate(mod, nsim = h, bootstrap = bootstrap, future = TRUE), simplify = "matrix")
    if(h ==1) {sim_coeffs <- matrix(sim_coeffs, nrow=1)}
    sim_coeffs_list[[j]] <- sim_coeffs  
  }
  
  #Pre-sample residual indices 
  resids_indices <- replicate(nsim, sample(ridx, h, replace = TRUE), simplify = FALSE)
  
  #Sample epsilon
  epsilon_bootstrap <- array(0, c(p, h, nsim))
  
  #For each age, bootstrap epsilon independently
  for (aa in 1:p) {
    epsilon_hat_i <- epsilon[aa, ]
    epsilon_hat_i <- na.omit(epsilon_hat_i)
    
    epsilon_bootstrap[aa, , ] <- replicate(nsim, sample(epsilon_hat_i, h, replace = TRUE))
  }
  
  
  output <- array(0,c(p,h,nsim))
  for(i in 1:nsim)
  {
    output[,,i] <- object$model$basis[,1]
    if(nb > 1)
    {
      for(j in 2:nb)
      {
        simulated_coeffs <- sim_coeffs_list[[j]][, i, drop=FALSE]
        output[,,i] <- output[,,i] + object$model$basis[,j] %*% matrix(simulated_coeffs, nrow = 1)
      }
    }
    output[,,i] <- output[,,i] + resids[,sample(ridx,h,replace=TRUE)]
    # if(adjust.modelvar)
    #   output[,,i] <- fmean + sweep(output[,,i]-fmean,1,sqrt(object$var$adj.factor),"*")
  }
  dimnames(output) <- list(object$age,object$year,1:nsim)
  
  lower_quantile <- apply(output, c(1, 2), quantile, probs = probs[1], na.rm = TRUE)
  upper_quantile <- apply(output, c(1, 2), quantile, probs = probs[2], na.rm = TRUE)
  
  dimnames(lower_quantile) <- list(object$age, object$year)
  dimnames(upper_quantile) <- list(object$age, object$year)
  
  if(adj_interval){
    m <- 30
    ferr <- object$error$y[,-(1:m)]
    quantiles_matrix <- t(apply(ferr, 1, function(x) {
      quantile(x, probs = probs, na.rm = TRUE)
    }))
    dx <- quantiles_matrix[,2] - quantiles_matrix[,1]
    u_plus_l <- upper_quantile + lower_quantile
    u_minus_l <- upper_quantile - lower_quantile
    px <- dx / u_minus_l[,1]
    upl <- sweep(u_minus_l, 1, px, "*")
    
    lower_quantile <- 0.5*u_plus_l - upl
    upper_quantile <- 0.5*u_plus_l + upl
    
  }
  return(list(lower = lower_quantile, upper = upper_quantile))
}

#Using intercept in linear regression
sigfdm2 <- function(data, series = names(data$rate)[3], order = 6, ages = data$age, 
                    max.age = max(ages), lambda = 3, mean = TRUE, level = FALSE, transform = TRUE,
                    m = 3, var.explained){
  
  # Check if the series is in the data
  if(!is.element(toupper(series), toupper(names(data$rate)))) {
    stop(paste("Series", series, "not found"))
  }
  
  # Get the series from the data
  i <- match(toupper(series), toupper(names(data$rate)))
  series_data <- as.matrix(data$rate[[i]])
  
  # Transform the mortality rate series using BoxCox transformation
  mx <- BoxCox(series_data, data$lambda)
  mx[mx < -1e+09] <- NA
  
  # Create functional time series object
  data.fts <- fts(ages, mx, start = data$year[1], xname = "Age", yname = "Mortality rate")
  ytsp <- data.fts$time
  char_value = colnames(data.fts$y)
  year <- as.numeric(substring(char_value, 1, 4))
  colnames(data.fts$y) = year
  
  ngrid <- max(500, ncol(data.fts$y))
  n <- ncol(data.fts$y)
  mm <- length(data.fts$x)
  x <- 1:mm
  yy <- matrix(NA, nrow = ngrid, ncol = n)
  
  # Apply spline interpolation to the data
  for(i in 1:n) {
    yy[,i] <- spline(x, data.fts$y[,i], n = ngrid)$y
  }
  xx <- seq(min(x), max(x), l = ngrid)
  
  # Calculate the mean and standard error of the mortality rates
  ax <- rowMeans(yy, na.rm = TRUE)
  axse <- approx(xx, sqrt(apply(yy, 1, var) / n), xout = x)$y
  
  # Initialize basis and coefficient matrices
  coeff <- basis <- NULL
  coeff <- cbind(rep(1, n), coeff)
  basis <- cbind(approx(xx, ax, xout = x)$y, basis)
  colnames(coeff)[1] <- colnames(basis)[1] <- "mean"
  
  # Mean-adjusted mortality rates
  Y <- sweep(yy, 1, ax)
  
  # Calculate signature of paths for dimensionality reduction
  d <- 3
  sig <- matrix(0, nrow = nrow(yy), ncol = ((d^(m + 1) - 1) / (d - 1)) - 1)
  for(i in 1:nrow(yy)) {
    sig[i,] <- tsig(embed_path(Y[i,]), m)
  }
  
  # Principal Component Analysis on the signature
  pca <- prcomp.sig(sig)
  variance <- pca$sdev^2
  prop_variance <- variance / sum(variance)
  cumulative_variance <- cumsum(prop_variance)
  
  if(is.null(order)) {
    order <- which(cumulative_variance >= var.explained)[1]
  }
  order <- min(order, ncol(pca$x))
  
  # Principal Component Analysis on the signature
  sigphi <- prcomp.sig(sig)[["x"]][,1:order]
  beta <- matrix(0, nrow = n, ncol = order)
  
  # Linear modeling of the mean-adjusted rates
  for(i in 1:n) {
    cf <- lm(Y[,i] ~ sigphi)$coefficients
    cof <- cf[!is.na(cf)]
    cof <- c(cof[names(cof) != "(Intercept)"], cof["(Intercept)"])
    beta[i,] <- cof
  }
  del <- which(is.na(cf))-1
  sigphi2 <- matrix(NA, nrow = length(x), ncol = order)
  for(i in 1:order) {
    sigphi2[,i] <- approx(xx, sigphi[,i], xout = x)$y
  }
  sigphi2 <- sigphi2[,-del]
  basis <- cbind(basis, sigphi2,1)
  coeff <- cbind(coeff, beta)
  colnames(basis)[2:(order + 1)] <- paste("Z", 1:order, sep = "")
  colnames(coeff)[2:(order + 1)] <- paste("beta", 1:order, sep = "")
  
  # Calculate fitted values
  fitted <- basis %*% t(coeff)
  colnames(fitted) <- colnames(mx)
  rownames(fitted) <- rownames(mx)
  
  # Create functional time series for the fitted values
  fits <- fts(data.fts$x, fitted, start = ytsp[1], frequency = 1, xname = data.fts$xname,
              yname = paste("Fitted", data.fts$yname))
  res <- fts(data.fts$x, data.fts$y - fitted)
  
  ov <- data[["obs.var"]][[series]]
  value <- switch(series,"male" = 1,"female" = 2,"total" = 3)
  s_ep <- data[["err"]][[value]][,1:dim(ov)[2]]
  
  epsil <- s_ep/sqrt(ov)
  return(list(sig = sig, basis = basis, coef = coeff, fitted = fits, year = data$year, 
              ages = ages, Y = Y, mean.se = axse, residuals = res, y = data.fts, obs.var = ov,
              pop = data$pop$total, epsilon = epsil))
}



#Add thershold to original
sigfdm4 <- function(data, series = names(data$rate)[3], order = 6, ages = data$age, 
                    max.age = max(ages), lambda = 3, mean = TRUE, level = FALSE, transform = TRUE,
                    m = 3, var.explained){
  
  # Check if the series is in the data
  if(!is.element(toupper(series), toupper(names(data$rate)))) {
    stop(paste("Series", series, "not found"))
  }
  
  # Get the series from the data
  i <- match(toupper(series), toupper(names(data$rate)))
  series_data <- as.matrix(data$rate[[i]])
  
  # Transform the mortality rate series using BoxCox transformation
  mx <- BoxCox(series_data, data$lambda)
  mx[mx < -1e+09] <- NA
  
  # Create functional time series object
  data.fts <- fts(ages, mx, start = data$year[1], xname = "Age", yname = "Mortality rate")
  ytsp <- data.fts$time
  char_value = colnames(data.fts$y)
  year <- as.numeric(substring(char_value, 1, 4))
  colnames(data.fts$y) = year
  
  ngrid <- max(500, ncol(data.fts$y))
  n <- ncol(data.fts$y)
  mm <- length(data.fts$x)
  x <- 1:mm
  yy <- matrix(NA, nrow = ngrid, ncol = n)
  
  # Apply spline interpolation to the data
  for(i in 1:n) {
    yy[,i] <- spline(x, data.fts$y[,i], n = ngrid)$y
  }
  xx <- seq(min(x), max(x), l = ngrid)
  
  # Calculate the mean and standard error of the mortality rates
  ax <- rowMeans(yy, na.rm = TRUE)
  axse <- approx(xx, sqrt(apply(yy, 1, var) / n), xout = x)$y
  
  # Initialize basis and coefficient matrices
  coeff <- basis <- NULL
  coeff <- cbind(rep(1, n), coeff)
  basis <- cbind(approx(xx, ax, xout = x)$y, basis)
  colnames(coeff)[1] <- colnames(basis)[1] <- "mean"
  
  # Mean-adjusted mortality rates
  Y <- sweep(yy, 1, ax)
  
  # Calculate signature of paths for dimensionality reduction
  d <- 3
  sig <- matrix(0, nrow = nrow(yy), ncol = ((d^(m + 1) - 1) / (d - 1)) - 1)
  for(i in 1:nrow(yy)) {
    sig[i,] <- tsig(embed_path(Y[i,]), m)
  }
  
  pca <- prcomp.sig(sig)
  variance <- pca$sdev^2
  prop_variance <- variance / sum(variance)
  cumulative_variance <- cumsum(prop_variance)
  
  if(is.null(order)) {
    order <- which(cumulative_variance >= var.explained)[1]
  }
  order <- min(order, ncol(pca$x))
  
  # Principal Component Analysis on the signature
  sigphi <- prcomp.sig(sig)[["x"]][,1:order]
  beta <- matrix(0, nrow = n, ncol = order)
  
  # Linear modeling of the mean-adjusted rates
  for(i in 1:n) {
    beta[i,] <- lm(Y[,i] ~ sigphi - 1)$coefficients
  }
  sigphi2 <- matrix(NA, nrow = length(x), ncol = order)
  for(i in 1:order) {
    sigphi2[,i] <- approx(xx, sigphi[,i], xout = x)$y
  }
  basis <- cbind(basis, sigphi2)
  coeff <- cbind(coeff, beta)
  colnames(basis)[2:(order + 1)] <- paste("Z", 1:order, sep = "")
  colnames(coeff)[2:(order + 1)] <- paste("beta", 1:order, sep = "")
  
  # Calculate fitted values
  fitted <- basis %*% t(coeff)
  colnames(fitted) <- colnames(mx)
  rownames(fitted) <- rownames(mx)
  
  # Create functional time series for the fitted values
  fits <- fts(data.fts$x, fitted, start = ytsp[1], frequency = 1, xname = data.fts$xname,
              yname = paste("Fitted", data.fts$yname))
  res <- fts(data.fts$x, data.fts$y - fitted)
  
  ov <- data[["obs.var"]][[series]]
  value <- switch(series,"male" = 1,"female" = 2,"total" = 3)
  s_ep <- data[["err"]][[value]][,1:dim(ov)[2]]
  
  epsil <- s_ep/sqrt(ov)
  return(list(sig = sig, basis = basis, coef = coeff, fitted = fits, year = data$year, 
              ages = ages, Y = Y, mean.se = axse, residuals = res, y = data.fts, obs.var = ov,
              pop = data$pop$total, epsilon = epsil))
}

#No smoothing
sigfdmNS <- function(data, series = names(data$rate)[3], order = 6, ages = data$age, 
                     max.age = max(ages), lambda = 3, mean = TRUE, level = FALSE, transform = TRUE,
                     m = 3){
  
  # Check if the series is in the data
  if(!is.element(toupper(series), toupper(names(data$rate)))) {
    stop(paste("Series", series, "not found"))
  }
  
  # Get the series from the data
  i <- match(toupper(series), toupper(names(data$rate)))
  series_data <- as.matrix(data$rate[[i]])
  
  # Transform the mortality rate series using BoxCox transformation
  mx <- BoxCox(series_data, data$lambda)
  mx[mx < -1e+09] <- NA
  
  # Create functional time series object
  data.fts <- fts(ages, mx, start = data$year[1], xname = "Age", yname = "Mortality rate")
  ytsp <- data.fts$time
  char_value = colnames(data.fts$y)
  year <- as.numeric(substring(char_value, 1, 4))
  colnames(data.fts$y) = year
  
  #ngrid <- max(500, ncol(data.fts$y))
  n <- ncol(data.fts$y)
  mm <- length(data.fts$x)
  x <- 1:mm
  yy <- data.fts$y
  xx <- x
  
  # Calculate the mean and standard error of the mortality rates
  ax <- rowMeans(yy, na.rm = TRUE)
  axse <- approx(xx, sqrt(apply(yy, 1, var) / n), xout = x)$y
  
  # Initialize basis and coefficient matrices
  coeff <- basis <- NULL
  coeff <- cbind(rep(1, n), coeff)
  basis <- cbind(ax, basis)
  colnames(coeff)[1] <- colnames(basis)[1] <- "mean"
  
  # Mean-adjusted mortality rates
  Y <- sweep(yy, 1, ax)
  
  # Calculate signature of paths for dimensionality reduction
  d <- 3
  sig <- matrix(0, nrow = nrow(yy), ncol = ((d^(m + 1) - 1) / (d - 1)) - 1)
  for(i in 1:nrow(yy)) {
    sig[i,] <- tsig(embed_path(Y[i,]), m)
  }
  
  # Principal Component Analysis on the signature
  sigphi <- prcomp.sig(sig)[["x"]][,1:order]
  beta <- matrix(0, nrow = n, ncol = order)
  
  # Linear modeling of the mean-adjusted rates
  for(i in 1:n) {
    beta[i,] <- lm(Y[,i] ~ sigphi - 1)$coefficients
  }
  sigphi2 <- matrix(NA, nrow = length(x), ncol = order)
  for(i in 1:order) {
    sigphi2[,i] <- approx(xx, sigphi[,i], xout = x)$y
  }
  basis <- cbind(basis, sigphi2)
  coeff <- cbind(coeff, beta)
  colnames(basis)[2:(order + 1)] <- paste("Z", 1:order, sep = "")
  colnames(coeff)[2:(order + 1)] <- paste("beta", 1:order, sep = "")
  
  # Calculate fitted values
  fitted <- basis %*% t(coeff)
  colnames(fitted) <- colnames(mx)
  rownames(fitted) <- rownames(mx)
  
  # Create functional time series for the fitted values
  fits <- fts(data.fts$x, fitted, start = ytsp[1], frequency = 1, xname = data.fts$xname,
              yname = paste("Fitted", data.fts$yname))
  res <- fts(data.fts$x, data.fts$y - fitted)
  
  #ov <- data[["obs.var"]][[series]]
  value <- switch(series,"male" = 1,"female" = 2,"total" = 3)
  #s_ep <- data[["err"]][[value]][,1:dim(ov)[2]]
  
  #epsil <- s_ep/sqrt(ov)
  return(list(sig = sig, basis = basis, coef = coeff, fitted = fits, year = data$year, 
              ages = ages, Y = Y, mean.se = axse, residuals = res, y = data.fts, #obs.var = ov,
              pop = data$pop$total))
}
#No smoothing forecast
sigforecastNS <- function(object, h, level = c(80, 95), adjust = TRUE) {
  nb <- ncol(object$basis)
  order <- nb - 1
  l <- nrow(object$coef)
  
  fc_beta <- varfcast <- matrix(NA, nrow = h, ncol = ncol(object$coef))
  qconf <- qnorm(0.5 + level / 200)
  
  fitted <- matrix(1, nrow = l, ncol = ncol(object$coef))
  fmodels <- list()
  
  # Forecast ARIMA models for each coefficient
  for (i in 2:ncol(object$coef)) {
    barima <- auto.arima(ts(object$coef[,i]))
    #barima <- arima(ts(object$coef[,i]), order=c(0,1,0))
    fitted[,i] <- fitted(barima)
    pred <- forecast(barima, h = h, level = level)
    fc_beta[,i] <- pred$mean
    varfcast[,i] <- ((pred$upper[,1] - pred$lower[,1]) / (2 * qconf[1]))^2
    fmodels[[i]] <- pred
  }
  varfcast[,1] <- 0
  fc_beta[,1] <- 1
  
  # Generate forecasts
  fc <- object$basis %*% t(fc_beta)
  colnames(fc) <- (tail(object$year, 1) + 1):(tail(object$year, 1) + h)
  rownames(fc) <- rownames(object$fitted$y)
  
  ytsp <- tsp(object$fitted$time)
  error <- ts(object$coef - fitted, start = ytsp[1], frequency = ytsp[3])
  
  # One-step forecasts and errors
  ferror <- onestepfcast <- object$y
  onestepfcast$y <- object$basis %*% t(fitted)
  onestepfcast$yname <- "One step forecasts"
  colnames(onestepfcast$y) <- colnames(object$y$y)
  
  ferror$y <- object$y$y - onestepfcast$y
  ferror$yname <- "One step errors"
  
  fmean <- fts(object$y$x, fc, start = ytsp[2], frequency = ytsp[3], xname = object$y$xname,
               yname = "Forecasts")
  
  # res <- object$residuals
  # res2 <- res$y^2
  # vx <- rowMeans(res2, na.rm = TRUE)
  # modelvar <- object$basis^2 %*% t(varfcast)
  # totalvar <- sweep(modelvar, 1, vx + object$mean.se^2, "+")
  # if (adjust & nb > 1) {
  #   adj.factor <- rowMeans(ferror$y^2, na.rm = TRUE)/totalvar[,1]
  #   totalvar <- sweep(totalvar, 1, adj.factor, "*")
  # }
  # else adj.factor <- 1
  # s <- sqrt(abs(object$obs.var))
  # ysd <- s * NA
  # for (i in 1:ncol(ysd)) {
  #   if (sum(!is.na(s[,i])) >= 2) {
  #     ysd[,i] <- spline(object$ages, s[,i], n = nrow(totalvar))$y
  #   }
  # }
  # ysd <- rowMeans(ysd, na.rm = TRUE)
  # observ <- ysd^2
  # totalvar <- sweep(totalvar, 1, observ, "+")
  # 
  # 
  # fse80 <- qnorm(0.5 + level[1] / 200) * sqrt(totalvar)
  # fse95 <- qnorm(0.5 + level[2] / 200) * sqrt(totalvar)
  # lower80 <- fmean$y - fse80
  # upper80 <- fmean$y + fse80
  # lower95 <- fmean$y - fse95
  # upper95 <- fmean$y + fse95
  
  
  return(list(forecast = fmean$y, fc_beta = fc_beta, h = h, 
              fc_year = max(object$year)+(1:h)/stats::frequency(object$year),
              model_sigfdm = object)) 
  #lower = list(lower80 = lower80, lower95 = lower95), 
  #upper = list(upper80 = upper80, upper95 = upper95), ferror = ferror,
  #var = list(model = modelvar,error = vx, mean = object$mean.se^2, 
  #           total = totalvar, coeff = varfcast, observ=observ,
  #           adj.factor = adj.factor),
  #coef_model = fmodels))
  
  
}
