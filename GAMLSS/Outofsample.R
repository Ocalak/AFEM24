#Unfortonuately I have to fit the mdeol and made the predictions manually due to an error in gamlss.
#If i define window_l= 34444:53000 and use it in gamlss() as gamlss(,data=data[window_l,] it gives error. 
predictions_oos <- function(data,Dist)
cont = gamlss.control(c.crit = 0.01)

Y <- model.matrix(~(Hod+Hoy+How+HolFix+HolFlx+XHol+Temp_Lag2+Temp_Lag4+Temp_Lag24+Temp_ACT+Temp_Lag1+HDD+CDD+Temp_mean24+Temp_8+(Hoy+Hod+How)^2
                         Temp_Lag168 +Load_Lag168+ Load_Lag144+Load_Lag48+Load_Lag24+Temp_max+Temp_min),data=data[(36488:54008),])


ultra1 <- data.frame("Load_DA"=newdflasso$Load_DA[(36488:54008)], Y[,-1])


if(Dist != "NO"){
Lasso_Ext_Selection <- gamlss(Load_DA~gnet(x.vars=names(ultra1)[-c(1)],method = "IC", ICpen="BIC"),
                   sigma.fo= ~gnet(x.vars=  names(ultra1)[-c(1)],method = "IC", ICpen="BIC"),
                   nu.fo= ~gnet(x.vars=  names(ultra1)[-c(1)],method = "IC", ICpen="BIC"),
                  tau.fo=~gnet(x.vars=  names(ultra1)[-c(1)],method = "IC", ICpen="BIC"),
                   data=ultra1, family=NO,method = RS(100),control=cont)
}else{
  Lasso_Ext_Selection <- gamlss(Load_DA~gnet(x.vars=names(ultra1)[-c(1)],method = "IC", ICpen="BIC"),
                   sigma.fo= ~gnet(x.vars=  names(ultra1)[-c(1)],method = "IC", ICpen="BIC"),
                   data=ultra1, family=NO,method = RS(100),control=cont)
  
}


# Construct the formula string for mu

mu_formula <- paste("Load_DA ~", paste(paste0("pb(", names(which(tail(getSmo(lasoo_no, "mu"), 1)[[1]]$beta != 0)), ")"), collapse = " + "))

sigma_formula <- paste("~", paste(paste0("pb(", names(which(tail(getSmo(lasoo_no, "sigma"), 1)[[1]]$beta != 0)), ")"), collapse = " + "))

mu_formula <- as.formula(mu_formula)

sigma_formula <- as.formula(sigma_formula)


new_model <- gamlss(mu_formula,
                    sigma.fo=sigma_formula,
                    family=NO,
                    data=newdflasso[(36488:54008),],
                    method=RS(50),control =cont)#,c.crit=0.1).  
#save(new_model,file="~/Desktop/AFEM24/Normaldistfit1.RData")


pre_NO_3 <- matrix(nrow=60,ncol=168)
for (i in 0:29) {
  dist_pre <- predictAll(new_model, newdata = newdflasso[(53289+i*24):(53288+168+i*24),])
  pre_NO_3[i*2+1,1:168] <- dist_pre$mu[1:168]%>%exp()
  pre_NO_3[i*2+2,1:168] <- dist_pre$sigma[1:168]%>%exp()
} 
pre_NO_3 <- t(pre_NO_3) %>% data.frame()
write_csv(pre_NO_3,file="~/Desktop/AFEM24/NO-Test/Pe_NO_3.csv")

rm(new_model)
