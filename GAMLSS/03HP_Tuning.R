#Window     12.31.2020-12.31.2022
#Valdation: 12.31-2022-12.31.2023
#Test.      12.31.2023-01.06.2024


#Hyperparameter Tuning with lasso
HP_DIST <- function(data=NULL,window_l=NULL,Dist=NULL){
  
  X <- model.matrix(~(Temp_ACT+Hod+Hoy+How+HolFix+HolFlx+Temp_Lag1+XHol+Temp_mean+Temp_Lag1+  Temp_Lag2 +  Temp_Lag3+Temp_Lag4+Temp_Lag24+Temp_Lag48 +
                        Temp_Lag168 +Load_Lag168+ Load_Lag144 +Load_Lag120+ Load_Lag96  +Load_Lag72  +Load_Lag48+ trend+
                        Load_Lag24+  Temp_max + Temp_min,data=newdf[window_l,])
  
  HP_DF  <- data.frame("Load_DA"=x$Load_DA[window_l], X[,-1])

   lassofit <- gamlss(Load_DA~gnet(x.vars=names(HP_DF)[-c(1)],method = "IC", ICpen="BIC"),
         sigma.fo= ~gnet(x.vars=names(HP_DF)[-c(1)],method = "IC", ICpen="BIC"),
         nu.fo= ~gnet(x.vars=names(HP_DF)[-c(1)],method = "IC", ICpen="BIC"),
         tau.fo= ~gnet(x.vars=names(HP_DF)[-c(1)],method = "IC", ICpen="BIC"),
         data=HP_DF,family=Dist, 
                      method=mixed(100,100),#Increase n.cyc  if it does not converge.
                      control=gamlss.control(c.crit=0.1))
  
  
  
  
                    
                    
  # Extract the names of the significant coefficients
  #significant_coefs <- names(which(tail(getSmo(lassofit, "mu"), 1)[[1]]$beta != 0))

  
  # Construct the formula string for sigma,mu,tau,nu
  mu.formula <- paste("Load_DA~ ", paste(paste0("pb(", names(which(tail(getSmo(lassofit, "mu"), 1)[[1]]$beta != 0)), ")"), collapse = " + "))
                    
  sigma.formula <- paste("~", paste(paste0("pb(", names(which(tail(getSmo(lassofit, "sigma"), 1)[[1]]$beta != 0)), ")"), collapse = " + "))
                    
   nu.formula <- paste("~", paste(paste0("pb(", names(which(tail(getSmo(lassofit, "nu"), 1)[[1]]$beta != 0)), ")"), collapse = " + "))
                    
  tau.formula <- paste("~", paste(paste0("pb(", names(which(tail(getSmo(lassofit, "tau"), 1)[[1]]$beta != 0)), ")"), collapse = " + "))

                
  # Convert  strings to formulas
  mu_formula <- as.formula(mu.formula)
  sigma_formula <- as.formula(sigma.formula)
  nu_formula <- as.formula(nu.formula)
  tau_formula <- as.formula(tau.formula)
  
  # Fit the new GAMLSS model using the constructed formulas save it env.
  new_model <<- gamlss(mu_formula,
                       sigma.fo=sigma_formula,
                       nu_fo=nu_formula,
                       tau.fo=tau_formula,
                       family=Dist,#Link? 
                      data=data[window_l,],
                       method=mixed(100,100),#Increase n.cyc  if it does not converge.
                      control=gamlss.control(c.crit=0.1)))
  
}
