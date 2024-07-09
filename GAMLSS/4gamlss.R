gamlss_validation <- function(data,window_l,dist,){
  #Valdiaiton is 365 days. I fit the model every 30 days 

   Y <- model.matrix(~(Hod+Hoy+How+HolFix+HolFlx+XHol+Temp_Lag2+Temp_Lag4+Temp_Lag24+Temp_ACT+Temp_Lag1+
                        Temp_Lag168 +Load_Lag168+ Load_Lag144+Load_Lag48+Load_Lag24+Temp_max+Temp_min),data=data[(window_l)+(t*24),])
  
  ultra1 <- data.frame("Load_DA"=data$Load_DA[(window_l)+(t*24)], Y[,-1])
  
  
  
  lasoo_no <- gamlss(Load_DA~gnet(x.vars=names(ultra1)[-c(1)],method = "IC", ICpen="BIC"),
                     sigma.fo= ~gnet(x.vars=  names(ultra1)[-c(1)],method = "IC", ICpen="BIC"),
                     data=ultra1, family=Dist,method = mixed(500,500),control=contr)
  
  
  # Construct the formula string for mu
  
  mu_formula <- paste("Load_DA ~", paste(paste0("pb(", names(which(tail(getSmo(lasoo_no, "mu"), 1)[[1]]$beta != 0)), ")"), collapse = " + "))
  
  sigma_formula <- paste("~", paste(paste0("pb(", names(which(tail(getSmo(lasoo_no, "sigma"), 1)[[1]]$beta != 0)), ")"), collapse = " + "))
  
  mu_formula <- as.formula(mu_formula)
  
  sigma_formula <- as.formula(sigma_formula)
  
  
  new_model <- gamlss(mu_formula,sigma.fo=sigma_formula,family=Dist, data=data[(window_l)+t*24),],method=mixed(50,50),control = gamlss.control(c.crit = 0.1))  
  
  #save(new_model,file="~/Desktop/AFEM24/Normaldistfit1.RData")
  
  if(new_model$converge == FALSE){
    refit(new_model)
  }
  #idx <- length(unique(yday(data$DateTime[43089:51848])))# == "2023-12-01 00:00:00")
  
  for (i in 0:29) {
    pred <-  predictAll(new_model,newdata=data[(43809+t*24+i*24):(43976+t*24+i*24),])
    predictions[i*2+1,1:168] <-  pred$mu[1:168]%>%exp()
    predictions[i*2+2,1:168] <-  pred$sigma[1:168]%>%exp()
  }
  predictions <<- t(predictions) %>%as.data.frame()%>%t()
  #write_csv(val_NO_mu1sd1,file= paste0("~/Desktop/AFEM24/NO-VAL/NO_VAL_PRE",t,".csv"))
}





  
