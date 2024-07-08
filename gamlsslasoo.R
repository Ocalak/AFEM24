

install.packages("gamlss.lasso")
library(gamlss.lasso)
X <- model.matrix(~(Temp_ACT+Hod+Hoy+How+HolFix+HolFlx+Temp_Lag1+XHol+Temp_mean+Temp_Lag1+  Temp_Lag2 +  Temp_Lag3+Temp_Lag4+Temp_Lag24+Temp_Lag48 +
                      Temp_Lag168 +Load_Lag168+ Load_Lag144 +Load_Lag120+ Load_Lag96  +Load_Lag72  +Load_Lag48+ trend+
                      Load_Lag24+  Temp_max + Temp_min),data=newdf[(25568+720):(43088+720),])


ultra1 <- data.frame("Load_DA"=newdf$Load_DA[(25568+720):(43088+720)], X[,-1])



lasoo_no <- gamlss(Load_DA~gnet(x.vars=names(ultra1)[-c(1)],method = "IC", ICpen="BIC"),
       sigma.fo= ~gnet(x.vars=  names(ultra1)[-c(1)],method = "IC", ICpen="BIC"),
       data=ultra1,family=NO(mu.link="log"),method =mixed(2000,1100)
                   ,c.crit=0.1)#,i.control = glim.control(cyc=1, bf.cyc=1))


names(which(tail(getSmo(lasoo_no, "mu") ,1)[[1]]$beta != 0))
names(which(tail(getSmo(lasoo_no, "sigma") ,1)[[1]]$beta != 0))

#lasoo_no
significant_coefs <- names(which(tail(getSmo(lasoo_no, "mu"), 1)[[1]]$beta != 0))
# Construct the formula string for mu
mu_formula <- paste("Load_DA~ ", paste(paste0("pb(", names(which(tail(getSmo(lasoo_no, "mu"), 1)[[1]]$beta != 0)), ")"), collapse = " + "))
sigma_formula <- paste("~", paste(paste0("pb(", names(which(tail(getSmo(lasoo_no, "sigma"), 1)[[1]]$beta != 0)), ")"), collapse = " + "))

mu_formula <- as.formula(mu_formula)
sigma_formula <- as.formula(sigma_formula)


new_model <- gamlss(mu_formula,sigma.fo=sigma_formula,family=NO(mu.link="log"), data=newdf[25568:43088,],method =mixed(2000,1100),c.crit=0.1)
