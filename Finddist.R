## Required packages.
install.packages("bamlss")
library("bamlss")
library("gamlss.dist")
library("parallel")

## Load the data.

data("fatalities", package = "bamlss")
fatalities$week
## Full model formula.
f <- list(
  Load_DA   ~ s(Hoy, bs = "cc", k = 20),
  sigma ~  s(Hoy, bs = "cc", k = 20),
  nu    ~  s(Hoy, bs = "cc", k = 20),
  tau   ~  s(Hoy, bs = "cc", k = 20)
)

## Setup function to run in parallel.
parallel_fun <- function(j) {
  cat("family", j, "\n")
  
  fam <- get(j)
  
  set.seed(123)
  b1 <- bamlss(Load_DA ~ 1, data = lassodf[169:43088,], family = fam(mu.link = "log"),
               n.iter = 100, burnin = 200, thin = 10)
  
  set.seed(456)
  b2 <- bamlss(f,  data = lassodf[169:43088,], family = fam(mu.link = "log"),
               n.iter = 100, burnin = 200, thin = 10)

  set.seed(789)
  k <- 10
  folds <- rep(1:k, length.out = nrow(lassodf[169:43088,]))
  crps <- NULL
  for(i in 1:k) {
    df <- subset(lassodf[169:43088,], folds != i)
    de <- subset(lassodf[169:43088,], folds == i)
    b3 <- bamlss(f, data = df, family = fam(mu.link = "log"),
                 n.iter = 100, burnin = 200, thin = 10)
    crps <- c(crps, CRPS(b3, newdata = de, FUN = identity))
  }
  
  rval <- list()
  rval$distribution <- j
  
  par <- predict(b1, type = "parameter", drop = FALSE)
  dic <- DIC(b1)
  waic <- WAIC(b1)
  dnum <- family(b1)$d(lassodf$Load_DA[169:43088,], par)
  cat(".. .. b1: DIC =", dic$DIC, "pd =", dic$pd, "WAIC =", waic$WAIC1, "\n")
  rval$dic1 <- dic
  rval$waic1 <- waic$WAIC1
  rval$dnum <- dnum
  rval$b1 <- b1
  
  dic <- DIC(b2)
  waic <- WAIC(b2)
  cat(".. .. b2: DIC =", dic$DIC, "pd =", dic$pd, "WAIC =", waic$WAIC1, "\n")
  rval$dic2 <- dic
  rval$waic2 <- waic$WAIC1
  rval$b2 <- b2
  rval$crps <- mean(crps)
  
  return(rval)
}

## Families.
families <- c("NO", "JSU", "SHASH", "ST")

## Estimate models.
res <- mclapply(families, parallel_fun, mc.cores = length(families))
?mclapply()
## Save results.
save(res, file = "fatalities_models.rda")
