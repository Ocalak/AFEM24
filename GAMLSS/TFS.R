for (dp in 0:11) {
  for (zx in 0:29) {#data <- newdflasso
    
    
    window_l <- ((25568:43088)+720*dp+24*zx)
    X <- data$Load_DA[window_l]
    #time.tmp.num <- as.numeric(data$DateTime[window_l])
    #time.ext <- c(time.tmp.num, max(time.tmp.num) + 1:H * (3600 * 24) / S)
    #time.ext.utc <- as.POSIXct(time.ext, origin = "1970-01-01", tz = "UTC")
    
    
    How <- data$How[min(window_l):(max(window_l)+H)]
    Hoy <- data$Hoy[min(window_l):(max(window_l)+H)]
    Hod <- data$Hod[min(window_l):(max(window_l)+H)]
    HolFlx <- data$HolFlx[min(window_l):(max(window_l)+H)]
    HolFix <- data$HolFix[min(window_l):(max(window_l)+H)]
    Temp_lag4 <- data$Temp_Lag4[min(window_l):(max(window_l)+H)]
    Temp_min <- data$Temp_min[min(window_l):(max(window_l)+H)]
    Temp_Lag168 <- data$Temp_Lag168[min(window_l):(max(window_l)+H)]
    XHol <-    data$XHol[min(window_l):(max(window_l)+H)]
    Temp_Lag1 <- data$Temp_Lag1[min(window_l):(max(window_l)+H)]
    Temp_mean <- data$Temp_mean[min(window_l):(max(window_l)+H)]
    Temp_Lag2 <- data$Temp_Lag2[min(window_l):(max(window_l)+H)]
    Temp_Lag3 <- data$Temp_Lag3[min(window_l):(max(window_l)+H)]
    
    Temp_Lag8 <- data$Temp_Lag8[min(window_l):(max(window_l)+H)]
    Temp_Lag24 <- data$Temp_Lag24[min(window_l):(max(window_l)+H)]
    Temp_Lag48   <- data$Temp_Lag48[min(window_l):(max(window_l)+H)]
    Temp_max   <- data$Temp_max[min(window_l):(max(window_l)+H)]
    trend  <- data$trend[min(window_l):(max(window_l)+H)]
    Temp_ACT <-  data$Temp_ACT[min(window_l):(max(window_l)+H)]
    
    
    param <-  matrix(, H, 4)#2 parameters of JSU 
    
    LAGS <- c(1,24,48,144,168,336)
    get.lagged <- function(lag, Z) c(rep(NA, lag), Z[1:(length(Z) + pmin(0, H - lag))], rep(NA, max(0, H - lag))) ## caution! modified
    XLAG.ext <- sapply(LAGS, get.lagged, Z = X)
    dimnames(XLAG.ext)[[2]] <- paste0("L", LAGS)
    
    XREG <- cbind(XLAG.ext,HolFix,HolFlx,Hoy,Hod,How,XHol,Temp_lag4,Temp_Lag1,Temp_Lag24,Temp_min,Temp_max)
    v_l <- ncol(XREG)
    
    
    ## create active lag sets
    LAGS.active <- list()
    for (h in 1:H) {
      LAGS.active[[h]] <- LAGS[LAGS >= h]
    }
    LAGS.active.length <- sapply(LAGS.active, length)
    
    IDX <- match(unique(LAGS.active.length), LAGS.active.length)
    LAGS.used <- list()
    H.used <- list()
    IDXH <- c(IDX - 1, H)
    for (i in seq_along(IDX)) {
      LAGS.used[[i]] <- LAGS.active[[IDX[i]]]
      H.used[[i]] <- (IDXH[i] + 1):IDXH[i + 1]
    }
    
    TAU <- seq(0.05,0.95,0.05)
    QUANTILES <- matrix(, H, length(TAU))
    
    contr <- gamlss.control(c.crit = 0.1,trace=FALSE)
    #i.used <- 2
    for (i.used in seq_along(H.used)) {
      LAGS.used.now <- LAGS.used[[i.used]] ## lag sets
      
      active.set <- match(LAGS.used.now, LAGS)
      Xid <- c(active.set, length(LAGS) + v_l-length(LAGS))
      DXREG <- cbind(Y = X, as.data.frame(head(XREG, length(X))[, Xid]))
      
      location_formula <- as.formula(paste("Y~",paste(paste0("pb(",dimnames(DXREG)[[2]][-1],",method='GAIC')", collapse = "+"))))
      #location_formula <- as.formula(paste("Y~",paste(paste0(dimnames(DXREG)[[2]][-1], collapse = "+"))))
      
      sigma_formula <- as.formula(paste("~",paste(paste0(dimnames(DXREG)[[2]][-1], collapse = "+"))))
      #sigma_formula <-   as.formula(paste("~",paste(paste0("pb(",dimnames(DXREG)[[2]][-1],",method='GAIC')", collapse = "+"))))
      nu_formula <-  as.formula(paste("~",paste(paste0(dimnames(DXREG)[[2]][-1], collapse = "+"))))
      #nu_formula <-   as.formula(paste("~",paste(paste0("pb(",dimnames(DXREG)[[2]][-1],",method='GAIC')", collapse = "+"))))
      tau_formula <-as.formula(paste("~",paste(paste0(dimnames(DXREG)[[2]][-1], collapse = "+"))))
      #tau_formula <- as.formula(paste("~",paste(paste0("pb(",dimnames(DXREG)[[2]][-1],",method='GAIC')", collapse = "+"))))
      
      model <- gamlss(location_formula,
                      sigma.fo=sigma_formula,
                      nu.fo=nu_formula,
                      tau.fo = tau_formula,
                      data = na.omit(DXREG),
                      method = RS(1),
                      family =SHASH(mu.link="log"),
                      control = contr)
      
      
      Xout <- as.data.frame(XREG[length(X) + H.used[[i.used]], Xid, drop = FALSE])
      Xpredlss <- predictAll(model, newdata = Xout) 
      
      QUANTILES[H.used[[i.used]], ] <- qJSU(rep(TAU, each = length(H.used[[i.used]])),
                                            rep(Xpredlss$mu, length(TAU)), 
                                            rep(Xpredlss$sigma, length(TAU)),
                                            rep(Xpredlss$nu, length(TAU)),
                                            rep(Xpredlss$tau, length(TAU)))
      
      param[H.used[[i.used]], 1] <- Xpredlss$mu
      param[H.used[[i.used]], 2] <- Xpredlss$sigma
      param[H.used[[i.used]], 3] <-  Xpredlss$nu
      param[H.used[[i.used]], 4] <- Xpredlss$tau
      
    }
    
    y <- newdflasso$Load_ACT[max(window_l)+1:168]
    
    QUANTILES.sorted <- t(apply(QUANTILES, 1, sort))
    Qlist <- list(QUANTILES.sorted)
    
    PBlist <- lapply(Qlist, function(QhatX) pinball(QhatX, y, TAU))
    #crps_all <- c()
    #for (i in 1:168) {
    #crps_all[i] <- crps_jsu(y[i], param[i,1], param[i,2],param[i,3],param[i,4])
    #}
    
    #mean_crps[zx+1] <- mean(crps_all)
    
    
    link1 <-  paste0("~/Desktop/AFEM24/SHASH-VAL/V_Param_SHASH",zx+1+30*dp,".csv")
    write.csv(data.frame(param),file=link1,row.names = FALSE)
    
    PB_LOSS[zx+1+dp*30] <- lapply(PBlist, mean)[[1]]
    if(zx == 29){
      link3 = paste0("~/Desktop/AFEM24/SHASH-VAL/VAL_PB_SHASH",zx+1+30*dp,".csv")
      write.csv(data.frame(PB_LOSS),file=link3,row.names = FALSE)
      
      #link4 <-  paste0("~/Desktop/AFEM24/JSU-VAL/V_CRPS_JSU",zx+1+30*dp,".csv")
      #write.csv(t(data.frame(mean_crps)),file=link4,row.names = FALSE)
    }
    
  }
}
