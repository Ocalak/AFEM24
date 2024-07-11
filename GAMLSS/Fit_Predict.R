Y <- model.matrix(~(Hod+Hoy+How+HolFix+HolFlx+XHol+Temp_Lag4+Temp_Lag24+Temp_ACT+Temp_Lag1+Load_Lag168+Load_Lag144+Load_Lag48+Load_Lag24
                   # +Temp_max
                    +Temp_min+trend),data=newdflasso[(25568+720):(43088+720),])

  sample_df <- data.frame("Load_DA"=newdflasso$Load_DA[(25568+720):(43088+720)], Y[,-1])
  contr <- gamlss.control(c.crit = 0.1)
  
  lasso_var_selec <- gamlss(Load_DA~gnet(x.vars=names(sample_df)[-c(1)],method = "IC", ICpen="BIC"),
                   sigma.fo= ~gnet(x.vars=  names(sample_df)[-c(1)],method = "IC", ICpen="BIC"),
                   nu.fo=~gnet(x.vars=  names(sample_df)[-c(1)],method = "IC", ICpen="BIC"),
                   tau.fo=~gnet(x.vars=  names(sample_df)[-c(1)],method = "IC", ICpen="BIC"),
                   data=sample_df, family=JSU,method = RS(50),control=contr)
  
  
   mu_formula <- paste("Load_DA ~", paste(paste0("pb(", names(which(tail(getSmo(  lasso_var_selec, "mu"), 1)[[1]]$beta != 0)), 
                                                 " ,method='GAIC')"), collapse = " + "))

   sigma_formula <- paste("~", paste(paste0("pb(", names(which(tail(getSmo(  lasso_var_selec, "sigma"), 1)[[1]]$beta != 0)),
                                           ",method='GAIC')"), collapse = " + "))
   
   nuu_formula <- paste("~", paste(paste0("pb(", names(which(tail(getSmo(  lasso_var_selec, "nu"), 1)[[1]]$beta != 0)),
                                           ",method='GAIC')"), collapse = " + "))
   tau_formula <- paste("~", paste(paste0("pb(", names(which(tail(getSmo(  lasso_var_selec, "tau"), 1)[[1]]$beta != 0)),
                                           ",method='GAIC')"), collapse = " + "))

  mu_formula <- as.formula(mu_formula)
  sigma_formula <- as.formula(sigma_formula)
  nu_formula <- as.formula(nu_formula)
  tau_formula <- as.formula(tau_formula)
  
  a <- names(which(tail(getSmo(  lasso_var_selec, "mu"), 1)[[1]]$beta != 0))
  b <- names(which(tail(getSmo(  lasso_var_selec, "sigma"), 1)[[1]]$beta != 0))
  c <- names(which(tail(getSmo(  lasso_var_selec, "nu"), 1)[[1]]$beta != 0))
  d <- names(which(tail(getSmo(  lasso_var_selec, "tau"), 1)[[1]]$beta != 0))

for (zx in 0:29) {
  
#}
#zx <- 0
H <- 168
window_l = (25568+zx*24):(43088+zx*24)
min(window_l)
max(window_l)
#f.distributionalReg_ARX_normal <- function(data,time, H, TAU,window_l) {#window_l = (25568+i*24):(43088+i*24) data <- newdflasso
  X <- newdflasso$Load_DA[window_l]
  time.tmp.num <- as.numeric(data$DateTime[window_l])
  time.ext <- c(time.tmp.num, max(time.tmp.num) + 1:H * (3600 * 24) / S)
  time.ext.utc <- as.POSIXct(time.ext, origin = "1970-01-01", tz = "UTC")
  
  
  How <- newdflasso$How[min(window_l):(max(window_l)+H)]
  Hoy <- newdflasso$Hoy[min(window_l):(max(window_l)+H)]
  Hod <- newdflasso$Hod[min(window_l):(max(window_l)+H)]
  HolFlx <- newdflasso$HolFlx[min(window_l):(max(window_l)+H)]
  HolFix <- newdflasso$HolFix[min(window_l):(max(window_l)+H)]
  Temp_lag4 <- newdflasso$Temp_Lag4[min(window_l):(max(window_l)+H)]
  Temp_min <- newdflasso$Temp_min[min(window_l):(max(window_l)+H)]
  Temp_Lag168 <- newdflasso$Temp_Lag168[min(window_l):(max(window_l)+H)]
  XHol <-    newdflasso$XHol[min(window_l):(max(window_l)+H)]
  Temp_Lag1 <- newdflasso$Temp_Lag1[min(window_l):(max(window_l)+H)]
  Temp_mean <- newdflasso$Temp_mean[min(window_l):(max(window_l)+H)]
  Temp_Lag2 <- newdflasso$Temp_Lag2[min(window_l):(max(window_l)+H)]
  Temp_Lag3 <- newdflasso$Temp_Lag3[min(window_l):(max(window_l)+H)]
  Temp_Lag5 <- newdflasso$Temp_Lag5[min(window_l):(max(window_l)+H)]
  Temp_Lag6 <- newdflasso$Temp_Lag6[min(window_l):(max(window_l)+H)]
  Temp_Lag7 <- newdflasso$Temp_Lag7[min(window_l):(max(window_l)+H)]
  Temp_Lag8 <- newdflasso$Temp_Lag8[min(window_l):(max(window_l)+H)]
  Temp_Lag24 <- newdflasso$Temp_Lag24[min(window_l):(max(window_l)+H)]
  Temp_Lag48   <- newdflasso$Temp_Lag48[min(window_l):(max(window_l)+H)]
  Temp_max   <- newdflasso$Temp_max[min(window_l):(max(window_l)+H)]
   trend  <- newdflasso$trend[min(window_l):(max(window_l)+H)]
 
  
  
  LAGS <- c(24,48,168)
  get.lagged <- function(lag, Z) c(rep(NA, lag), Z[1:(length(Z) + pmin(0, H - lag))], rep(NA, max(0, H - lag))) ## caution! modified
  XLAG.ext <- sapply(LAGS, get.lagged, Z = X)
  dimnames(XLAG.ext)[[2]] <- paste0("L", LAGS)
  
  XREG <- cbind(XLAG.ext,HolFix,HolFlx,Hoy,Hod,How,XHol,Temp_Lag24,trend)
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
  
 # contr <- gamlss.control(c.crit = 0.1)
  #i.used <- 1
  for (i.used in seq_along(H.used)) {
    LAGS.used.now <- LAGS.used[[i.used]] ## lag sets
    
    active.set <- match(LAGS.used.now, LAGS)
    Xid <- c(active.set, length(LAGS) + v_l-length(LAGS))
    DXREG <- cbind(Y = X, as.data.frame(head(XREG, length(X))[, Xid]))
    
    location_formula <- as.formula(paste("Y~",paste(paste0("pb(",dimnames(DXREG)[[2]][-1],",method='GAIC')", collapse = "+"))))
    sigma_formula <- as.formula(paste("~",paste(paste0(dimnames(DXREG)[[2]][-1], collapse = "+"))))
    nu_formula <-  as.formula(paste("~",paste(paste0(dimnames(DXREG)[[2]][-1], collapse = "+"))))
    tau_formula <-as.formula(paste("~",paste(paste0(dimnames(DXREG)[[2]][-1], collapse = "+"))))
    
    model <- gamlss(location_formula,
                    sigma.fo = sigma_formula,
                    nu.fo = nu_formula,
                    tau.fo = tau_formula,
                 data = na.omit(DXREG), 
                 family = JSU,
                 method=RS(100),
                 control = contr)
  
    
    Xout <- as.data.frame(XREG[length(X) + H.used[[i.used]], Xid, drop = FALSE])
    Xpredlss <- predictAll(model, newdata = Xout) 

    QUANTILES[H.used[[i.used]], ] <- qJSU(rep(TAU, each = length(H.used[[i.used]])),
                                          rep(Xpredlss$mu, length(TAU)), 
                                          rep(Xpredlss$sigma, length(TAU)),
                                         rep(Xpredlss$nu, length(TAU)),
                                         rep(Xpredlss$tau, length(TAU)))
    
    }
  
  QUANTILES.sorted <- t(apply(QUANTILES, 1, sort))
  
  #return(exp(QUANTILES.sorted))
  QQE <- exp(QUANTILES.sorted)
  
 paste0("write.csv(data.frame(QQE),file='~/Desktop/AFEM24/JSU-VAL/Q",i+1,".csv')")
 
#}
#pinball <- function(X, y, tau) t(t(y - X) * tau) * (y - X > 0) + t(t(X - y) * (1 - tau)) * (y - X < 0)

 y <- newdflasso$Load_ACT[max(window_l)+1:168]
  
  Qlist <- list(QQE)
  
  PBlist <- lapply(Qlist, function(Qhat) pinball(QQE, y, TAU))

  PB_LOSS[i+1] <- lapply(PBlist, mean)[[1]]
}
