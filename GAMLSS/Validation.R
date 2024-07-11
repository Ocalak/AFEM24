library(gamlss)
library(gamlss.dist)
library(gamlss.lasso)
library(dplyr)
library(lubridate)
library(gamlss.ggplots)





#01-12-2023----> OOS start
#01-12-2023---->In sample start
#01-12-2021-----> Fit starts


#I start with 0 so each to adjust by one.
month <- 0#December starting poinnt (2022-12-01) 0-11
for(month in 0:11)
#Variables_Lasso :The variables to tune in Adapt-Lasso.
 Y <- model.matrix(~(Variables_Lasso),#Holidays+Temerature+..............+Interaction terms How*Hod Hoy*Hod Hoy*How
                   data=data[(window)+720*month),])

#Create df for ADP-Lasso
sample_df <- data.frame("Load_DA"= data$Load_DA[(window)+720*month],
                        Y[,-1])


#Gamlss.control# It is recomennded by the Author for large datasets which have more than 10.0000 obs.
 contr <- gamlss.control(c.crit = 0.1,trace=FALSE)
   

#Variables selection
lasso_var_selec <- gamlss(Load_DA~gnet(x.vars=names(sample_df)[-c(1)],
                                       method = "IC", 
                                       ICpen="BIC",
                                      adaptive=1),
                   sigma.fo= ~gnet(x.vars=  names(sample_df)[-c(1)],
                                   method = "IC"
                                   , ICpen="BIC",
                                  adaptive=1),
                   data=sample_df, 
                          family=Dist,#Choose distribution. NO-JSU-SHASH-SEP3-4
                          method = mixed(50,50),
                          control=contr)


#Check worm plot##Additional
#wp(lasso_var_selec,ylim.all=3)
#plot(lasso_var_selec)



#§0 days of cycle with chosen paratmeters from Adp-Lasso.
for (zx in 1:29) {
  
  window_l <- (25568:43088)+720*month+24*zx)
  
  X <- data$Load_DA[window_l]
  #time.tmp.num <- as.numeric(data$DateTime[window_l])
  #time.ext <- c(time.tmp.num, max(time.tmp.num) + 1:H * (3600 * 24) / S)
  #time.ext.utc <- as.POSIXct(time.ext, origin = "1970-01-01", tz = "UTC")
  



  ######This step is necessary. 

  ##Choose the selected variables from Lasso.

  
 # How <- data$How[min(window_l):(max(window_l)+H)]
 # Hoy <- data$Hoy[min(window_l):(max(window_l)+H)]
 # Hod <- data$Hod[min(window_l):(max(window_l)+H)]
 # HolFlx <- data$HolFlx[min(window_l):(max(window_l)+H)]
 # HolFix <- data$HolFix[min(window_l):(max(window_l)+H)]
 # Temp_lag4 <- data$Temp_Lag4[min(window_l):(max(window_l)+H)]
 # Temp_min <- data$Temp_min[min(window_l):(max(window_l)+H)]
 # Temp_Lag168 <- data$Temp_Lag168[min(window_l):(max(window_l)+H)]
 # XHol <-    data$XHol[min(window_l):(max(window_l)+H)]
 # Temp_Lag1 <- data$Temp_Lag1[min(window_l):(max(window_l)+H)]
 # Temp_mean <- data$Temp_mean[min(window_l):(max(window_l)+H)]
 # Temp_Lag2 <- data$Temp_Lag2[min(window_l):(max(window_l)+H)]
 # Temp_Lag3 <- data$Temp_Lag3[min(window_l):(max(window_l)+H)]
 # Temp_Lag5 <- data$Temp_Lag5[min(window_l):(max(window_l)+H)]
 # Temp_Lag6 <- data$Temp_Lag6[min(window_l):(max(window_l)+H)]
 # Temp_Lag7 <- data$Temp_Lag7[min(window_l):(max(window_l)+H)]
 # Temp_Lag8 <- data$Temp_Lag8[min(window_l):(max(window_l)+H)]
 # Temp_Lag24 <- data$Temp_Lag24[min(window_l):(max(window_l)+H)]
 # Temp_Lag48   <- data$Temp_Lag48[min(window_l):(max(window_l)+H)]
 # Temp_max   <- data$Temp_max[min(window_l):(max(window_l)+H)]
 # trend  <- data$trend[min(window_l):(max(window_l)+H)]
 # Temp_ACT <-  data$Temp_ACT[min(window_l):(max(window_l)+H)]
  
  
  
  LAGS <- c(24,48,72,96,144,168) #Choose lags if any of them selected.
  get.lagged <- function(lag, Z) c(rep(NA, lag), Z[1:(length(Z) + pmin(0, H - lag))], rep(NA, max(0, H - lag)))
  
  XLAG.ext <- sapply(LAGS, get.lagged, Z = X)
  dimnames(XLAG.ext)[[2]] <- paste0("L", LAGS)
  
  XREG <- cbind(lasso_selected_features)
  
  v_l <- ncol(XREG)#number of selected features except lags
  
  
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
  

  
  for (i.used in seq_along(H.used)) {
    LAGS.used.now <- LAGS.used[[i.used]] ## lag sets
    
    active.set <- match(LAGS.used.now, LAGS)
    Xid <- c(active.set, length(LAGS) + v_l-length(LAGS))
    DXREG <- cbind(Y = X, as.data.frame(head(XREG, length(X))[, Xid]))


    #If is there any variables that shouldnt be in P-Splines than write it manually. This step need to be done manually. The script below is just an example. 
    #Each time or lets say each month I do  ADP-Lasso parameter selection. So, Here parameters would change. 
    ###Dont forget intercation terms. 
    
     location_formula <- as.formula(paste("Y~",paste(paste0("pb(",dimnames(DXREG)[[2]][-1],",method='GAIC')", 
                                                            collapse = "+"))))
    sigma_formula <-   as.formula(paste("~",paste(paste0("pb(",dimnames(DXREG)[[2]][-1],",method='GAIC')",
                                                         collapse = "+"))))
    nu_formula <-   as.formula(paste("~",paste(paste0("pb(",dimnames(DXREG)[[2]][-1],",method='GAIC')",
                                                      collapse = "+"))))
    tau_formula <- as.formula(paste("~",paste(paste0("pb(",dimnames(DXREG)[[2]][-1],",method='GAIC')", 
                                                     collapse = "+")))#


                              
    model <- gamlss(location_formula,
                    sigma.fo=sigma_formula,
                    nu.fo=nu_formula,
                    tau.fo = tau_formula,
                    data = na.omit(DXREG), 
                    family =JSU,
                    control = contr)
    

                              
    Xout <- as.data.frame(XREG[length(X) + H.used[[i.used]], Xid, drop = FALSE])
                              
    Xpredlss <- predictAll(model, newdata = Xout) 

     #If the choosen dist is NO, use : qnorm
                              #SHASH: qSHASH
                              #SEP : qSEP
    
    QUANTILES[H.used[[i.used]], ] <- qJSU(rep(TAU, each = length(H.used[[i.used]])),
                                          rep(Xpredlss$mu, length(TAU)), 
                                          rep(Xpredlss$sigma, length(TAU)),
                                         rep(Xpredlss$nu, length(TAU)),
                                         rep(Xpredlss$tau, length(TAU)))
    
  }
  
  QUANTILES.sorted <- t(apply(QUANTILES, 1, sort))
                              
  QQ_JSU_VAL<- exp(QUANTILES.sorted)
  #Save
  #link <-  paste0("~/Desktop/AFEM24/JSU-VAL/V_QQ_JSU",zx+1+30*month,".csv")
  #write.csv(data.frame(t(QQ_JSU_VAL)),file=link,row.names = FALSE)
  ############

                              #define y for PB
  y <- newdflasso$Load_ACT[max(window_l)+1:168]
  Qlist <- list(QQ_JSU_VAL)
  PBlist <- lapply(Qlist, function(QhatX) pinball(QhatX, y, TAU))
  
  #link1 <-  paste0("~/Desktop/AFEM24/JSU-VAL/V_PB_JSU",zx+1+30*month,".csv")
  #write.csv(t(data.frame(PBlist)),file=link1,row.names = FALSE)
  
  PB_LOSS[zx+1+month*30] <- lapply(PBlist, mean)[[1]]
  
}





                   
