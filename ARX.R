f.quantile_ARX_normal <- function(X,Y, time, H, TAU) {
  time.tmp.num <- as.numeric(time)
  time.ext <- c(time.tmp.num, max(time.tmp.num) + 1:H * (3600 * 24) / S)
  time.ext.utc <- as.POSIXct(time.ext, origin = "1970-01-01", tz = "UTC")
  
  DD <- as.numeric(format(time.ext.utc, "%w")) %in% c(0, 6) ## dummy
  
  LAGS <- c(24,48,144,168)
  get.lagged <- function(lag, Z) c(rep(NA, lag), Z[(1 + lag):(length(Z)) - lag])
  XLAG <- sapply(LAGS, get.lagged, Z = X)
  
  XLAG.ext <- rbind(XLAG, matrix(NA, H, length(LAGS)))
  XREG <- cbind(1, XLAG.ext, DD)
  p <- dim(XREG)[2] # p = number of regressors
  
  model <- lm(X ~ head(XREG, length(X)) - 1) ## lm can be replace by lm.fit when missing data is handled.
  
  betahat <- model$coef
  sigma2hat <- var(model$res)
  
  
  lagm <- max(LAGS) ## maximal memory
  M <- 10000
  
  ## now create large matrix with response and regression matrix
  XSIM <- array(cbind(c(tail(X, lagm), rep(NA, H)), tail(XREG, lagm + H)), dim = c(lagm + H, 1 + p, M))
  
  h <- 1
  EPS <- array(rnorm(H * M, sd = sqrt(sigma2hat)), dim = c(H, M))
  
  for (h in 1:H) {
    ## replace model specific unknown values with known ones (in the path)
    XSIM[lagm + h, 1 + 1 + 1:length(LAGS), ] <- XSIM[lagm + h - LAGS, 1, ]
    ## compute forecast:  beta'X_t + eps_t
    XSIM[lagm + h, 1, ] <- betahat %*% XSIM[lagm + h, 1 + 1:p, ] + EPS[h, ]
  } # h
  
  
  
  QUANTILES <- apply(XSIM[lagm + 1:H, 1, ], 1, quantile, probs = TAU)


  crps <- mean(crps_sample(Y,XSIM[lagm + 1:H, 1, ]))
  PB_ARX <- mean(colMeans(pinball(QUANTILES,Y,TAU),na.rm = TRUE))
  scores <- c(crps,PB_ARX)
  return(scores) 
}

REST_ARX <-  matrix(nrow=180,ncol=2)
for (t in 0:1179) {
REST_ARX[t,] <- f.quantile_ARX_normal(X=newdflasso$Load_DA[(window_l)+t*24],
                                   Y=newdflasso$Load_ACT[max(window_l)+t*24+1:168],
                                   time=newdflasso$DateTime[t*24+(window_l)],H=168,
                                   TAU)}



