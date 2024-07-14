

#Same apllies for Validation Set###Valdiation set is from 2022.12.01 to 2023.12.01 ## 

window_l <- 8048:43088# 2018.12.01 to 2023-12.01


Y <- model.matrix(~(Hod+Hoy+How+HolFix+HolFlx+XHol+Temp_Lag24+Temp_Lag1+Temp_Lag2+Temp_Lag3+Temp_mean3+
                    Load_Lag168+Load_Lag144+Load_Lag48+Load_Lag24+Temp_max+Temp_min+trend+(How+Hoy+Hod)^2),
                  data=newdflasso[(window_l)+720*K,])      #720 = 30 days * 24 hours, K is from 0 to 4 = December,January.March,April.May

  sample_df <- data.frame("Load_DA"=newdflasso$Load_DA[(25568:43088)+720*K)], Y[,-1])

  contr <- gamlss.control(c.crit = 0.1)
  
  lasso_var_selec <- gamlss(Load_DA~gnet(x.vars=names(sample_df)[-c(1)],method = "IC", ICpen="BIC"),
                   sigma.fo= ~gnet(x.vars=  names(sample_df)[-c(1)],method = "IC", ICpen="BIC"),
                   nu.fo=~gnet(x.vars=  names(sample_df)[-c(1)],method = "IC", ICpen="BIC"),
                   tau.fo=~gnet(x.vars=  names(sample_df)[-c(1)],method = "IC", ICpen="BIC"),
                   data=sample_df, family=NO(mu.link="log"),method = RS(100),control=contr)
  
  
 
#Print selected parameters
 names(which(tail(getSmo(  lasso_var_selec, "mu"), 1)[[1]]$beta != 0))
 names(which(tail(getSmo(  lasso_var_selec, "sigma"), 1)[[1]]$beta != 0)
 names(which(tail(getSmo(  lasso_var_selec, "nu"), 1)[[1]]$beta != 0)
 names(which(tail(getSmo(  lasso_var_selec, "tau"), 1)[[1]]$beta != 0)
       
pinball <- function(X, y, tau) t(t(y - X) * tau) * (y - X > 0) + t(t(X - y) * (1 - tau)) * (y - X < 0)


       
#loop for 30 days. We do that 12 times for validation set and 6 times for test set.
for (zx in 0:29) {
  
#}
#zx <- 0
H <- 168
window_l = (25568+zx*24):(43088+zx*24)

  
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
 
  
  
  LAGS <- c(24,48,144,168)#include here the lags
  get.lagged <- function(lag, Z) c(rep(NA, lag), Z[1:(length(Z) + pmin(0, H - lag))], rep(NA, max(0, H - lag))) ## caution! modified
  XLAG.ext <- sapply(LAGS, get.lagged, Z = X)
  dimnames(XLAG.ext)[[2]] <- paste0("L", LAGS)
  
  XREG <- cbind(XLAG.ext,HolFix,HolFlx,Hoy,Hod,How,XHol,Temp_Lag24,trend)#Include all the selected paramteters here wothput adding the lags I add the lags abov
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
    
#We add Load lags as linear term. we dont use smooth functions for them.
    formula_active <- paste("Y ~", paste(dimnames(DXREG)[[2]][active.set], collapse = "+"))

  # Generate the formula for inactive predictors using penalized splines with GAIC
    inactive_indices <- setdiff(seq_along(dimnames(DXREG)[[2]]), c(1, active.set))
    formula_inactive <- paste0("pb(", dimnames(DXREG)[[2]][inactive_indices], ", method='GAIC')", collapse = " + ")

    # Combine both formulas
    full_formula <- paste(formula_active, "+", formula_inactive)
    
    location_formula <- as.formula(full_formula)
    sigma_formula <- as.formula(full_formula)
    nu_formula <-  as.formula(full_formula)
    tau_formula <-as.formula(full_formula)
    
    model <- gamlss(location_formula,
                    sigma.fo = sigma_formula,
                    nu.fo = nu_formula,
                    tau.fo = tau_formula,
                 data = na.omit(DXREG), 
                 family = JSU(mu.link="log"),#Use NO for Normal dist, ST1 for Skewed t dist
                 method=RS(100),
                 control = contr)
    
    if(new_model$converge == FALSE){
    refit(model)
  }
  
    
    Xout <- as.data.frame(XREG[length(X) + H.used[[i.used]], Xid, drop = FALSE])
    Xpredlss <- predictAll(model, newdata = Xout) 

    QUANTILES[H.used[[i.used]], ] <- qJSU(rep(TAU, each = length(H.used[[i.used]])),
                                          rep(Xpredlss$mu, length(TAU)), 
                                          rep(Xpredlss$sigma, length(TAU)),
                                         rep(Xpredlss$nu, length(TAU)),
                                         rep(Xpredlss$tau, length(TAU)))
    # For normal dist
  #QUANTILES[H.used[[i.used]], ] <- qnorm(rep(TAU, each = length(H.used[[i.used]])),rep(Xpredlss$mu, length(TAU)),rep(Xpredlss$sigma, length(TAU))) # For normal dist


    #For Skewed t Dist 1
  #QUANTILES[H.used[[i.used]], ] <- qST(rep(TAU, each = length(H.used[[i.used]])),rep(Xpredlss$mu, length(TAU)),rep(Xpredlss$sigma, length(TAU)),rep(Xpredlss$nu, length(TAU)),rep(Xpredlss$tau, length(TAU)))
                                       
    
       #Save parameters
      param[H.used[[i.used]], 1] <- Xpredlss$mu
      param[H.used[[i.used]], 2] <- Xpredlss$sigma
      param[H.used[[i.used]], 3] <-  Xpredlss$nu
      param[H.used[[i.used]], 4] <- Xpredlss$tau
    
    }

  
  y <- newdflasso$Load_ACT[max(window_l)+1:168]
  
  QUANTILES.sorted <- t(apply(QUANTILES, 1, sort))

  
  Qlist <- list(QUANTILES.sorted)
  
  PBlist <- lapply(Qlist, function(Qhat) pinball(QQE, y, TAU))

  link1 <-  paste0("~/Desktop/AFEM24/SHASH-VAL/V_Param_JSU",zx+1+30*dp,".csv")
  write.csv(data.frame(param),file=link1,row.names = FALSE)

  
  PBlist <- lapply(Qlist, function(Qhat) pinball(QQE, y, TAU))

  PB_LOSS[i+1] <- lapply(PBlist, mean)[[1]]


  if(zx == 29){
      link3 = paste0("~/Desktop/AFEM24/SHASH-VAL/VAL_PB_SHASH",zx+1+30*dp,".csv")
      write.csv(data.frame(PB_LOSS),file=link3,row.names = FALSE)
  }
}
