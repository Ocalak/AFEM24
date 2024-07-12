library(qgam)
set.seed(2)
dat <- gamSim(1, n=300, dist="normal", scale=2)

fit <- mqgam(y~s(x0)+s(x1)+s(x2)+s(x3), data=dat, qu = c(0.2, 0.8))
fit <- mqgam(Load_DA~s(Hod,bs="cc",k=20)+s(Hoy,bs="cc",k=12), data=newdflasso[26300:43088,], qu = c(0.2, 0.8))
plot(fit)
invisible( qdo(fit, 0.2, plot, pages = 1) )

quSeq <- c(0.2,0.8)
set.seed(6436)
fit <- mqgam(accel~s(times, k=20, bs="ad"), data = mcycle, qu = quSeq)

# Plot the fit
xSeq <- data.frame(cbind("accel" = rep(0, 1e3), "times" = seq(2, 58, length.out = 1e3)))
plot(newdflasso$Hod[43089:43096], newdflasso$Load_DA[43089:43096])# xlab = "Times", ylab = "Acceleration", ylim = c(-150, 80))
for(iq in quSeq){
  pred <- qdo(fit, iq, predict, newdata = newdflasso[43089:43096,])
  lines(newdflasso$Hod[43089:43096], pred, col = 2)
}
exp(pred)

as.vector(exp(pred))- newdflasso[43089:43096,"Load_ACT"]

pred <- qdo(fit, quSeq, predict, newdata = newdflasso[43089,])
pred[[1]] newdflasso[43089:43096,]

pinball(c(exp(10.16),exp(10.43)),newdflasso$Load_ACT[43089],quSeq)
