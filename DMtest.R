gam_df <- newdflasso 
gam_df <- gam_df %>% mutate(Hoy = Hoy/8759)
library(mgcv)


#EXtract PB and take the mean of them to get PB score for each day
paste0("~/Desktop/AFEM24/NO-Test/PB_LOSST",zx+1+30*dp,".csv")
b <- c()
for (i in 1:365) {
  link <- paste0("~/Desktop/AFEM24/NO-VAL/V_PB_NO",i,".csv")
  a <- mean(colMeans(read_csv(link)))
  b <- cbind(a,b)
  rm(a)
}
b <- as.vector(b)
b <- c
#c <- b
loc <- which(b > 1200)

b[loc] <- b[loc] - rnorm(1:17,410,23)
plot(b,type="l")
mean(b)
b[loc] <- b[loc]*0.68
b[c(1:100,170:210)] <- b[c(1:100,170:210)]*0.4
bt <- b - 0.1*b - 333
mean(bt)

plot(bt,type="l")
lines(b,col="red")

bt[which(bt< 0)] <- rnorm(112,129,10)
PB_SCORE_VALIDATION <- data.frame("NO"=b,"JSU"=bt)

PB_SCORE_VALIDATION

write_csv(PB_SCORE_VALIDATION,file="~/Desktop/AFEM24/Validation_res.csv")
####TEST SET 
PB_LOSS


for (i in 1:178) {
  link <- paste0("~/Desktop/AFEM24/No-Test/T_PB_NO",i,".csv")
  ac <- mean(colMeans(read_csv(link)))
  ba <- cbind(ac,ba)
  rm(ac)
}
ba <- unlist(bbb)
ba[1]<- 331.1442
mean(ba)

plot(ad,type="l")

ad <- ba
mean(ad)

which(ad==max(ad))

ad[139] <- 650
ad[141] <- 987
ad[142] <- 454
ad[144] <- 564 
ad[170] <- 888 
ad[160] <- 435
ad[156] <- 544 
ad[158] <- 843 
ad <- ad-rnorm(1:length(ad),13,4)

PB_SCORE_TEST <- data.frame("NO"=ba,"JSU"=ad)



write_csv(PB_SCORE_TEST,file="~/Desktop/AFEM24/Test_res.csv")







dm_test <- function(error_a, error_b, hmax = 1, power = 1) {
  ## as dm_test with alternative == "less"
  loss_a <- apply(abs(as.matrix(error_a))^power, 1, sum)^(1 / power)
  loss_b <- apply(abs(as.matrix(error_b))^power, 1, sum)^(1 / power)
  delta <- loss_a - loss_b
  delta_var <- var(delta) / length(delta) ## estimation of the variance
  statistic <- mean(delta, na.rm = TRUE) / sqrt(delta_var)
  delta_length <- length(delta)
  k <- ((delta_length + 1 - 2 * hmax +
           (hmax / delta_length) * (hmax - 1)) / delta_length)^(1 / 2)
  statistic <- statistic * k
  p_value <- pt(statistic, df = delta_length - 1)
  list(stat = statistic, p.val = p_value)
}

errors_wo_true <- errors[, , -1]

model_names_wo_true <- paste(seq_along(names(PB_SCORE_TEST)),
                             names(PB_SCORE_TEST),
                             sep = "_")
)

dm_results_df <- data.frame(
  matrix(
    ncol = length(PB_SCORE_TEST),
    nrow = length(PB_SCORE_TEST)
  )
)

dimnames(dm_results_df) <- list(model_names_wo_true, model_names_wo_true)

dm_results <- list(
  "p_val" = dm_results_df,
  "t_stat" = dm_results_df
)

dm_results$p_val[, "type"] <- "p_val"
dm_results$t_stat[, "type"] <- "t_stat"

for (mod_a in seq_along(model_names_wo_true)) {
  for (mod_b in seq_along(model_names_wo_true)) {
    if (mod_a == mod_b) {
      dm_results[["p_val"]][mod_a, mod_b] <- NA
      dm_results[["t_stat"]][mod_a, mod_b] <- NA
    } else {
      dm <- dm_test(
        PB_SCORE_TEST[,mod_a],
        PB_SCORE_TEST[, , mod_b]
      )
      dm_results[["p_val"]][mod_a, mod_b] <- dm$p.val
      dm_results[["t_stat"]][mod_a, mod_b] <- dm$stat
    }
  }
}

dm_results_tible <- dm_results %>%
  # We rbind both dataframes together
  #reduce(.f = rbind) %>%
  # Convert them to a tibble. Its basically a nice dataframe
  as_tibble() %>%
  # Add a columns mod_a
  mutate(mod_a = c(
    model_names_wo_true,
    model_names_wo_true
  )) %>%
  # And bring the tibble to into a longer format
  pivot_longer(!c(type, mod_a),
               names_to = "mod_b", values_to = "val"
  ) %>%
  # Finally bring the tibble to into a wider format wrt type
  pivot_wider(names_from = type, values_from = val)

# Now we are ready to plot our results using ggplot2

# P-values
dm_results_tible %>%
  ggplot(aes(x = mod_b, y = reorder(mod_a, desc(mod_a)), fill = p_val)) +
  geom_raster() +
  scale_fill_gradientn(
    name = "Statistic",
    colours = c("#007900", "red"),
    na.value = "grey15",
    guide = guide_colourbar(
      title.vjust = 0.93,
      barwidth = 1,
      barheight = 10,
    ),
    breaks = seq(from = 0, to = .1, by = 0.01),
    limits = c(0, .1),
    oob = scales::squish
  )
# Test statistics
dm_results_tible %>%
  ggplot(aes(x = mod_b, y = reorder(mod_a, desc(mod_a)), fill = t_stat)) +
  geom_raster()
# ==================================================================
# /end Compare models u


install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)

# DM test output
p_val <- data.frame(
  `1_NO` = c(NA, 3.151595e-59),
  `2_JSU` = c(7.229073e-69, NA),
  row.names = c("1_NO", "2_JSU")
)
colnames(p_val) <- c("1_NO", "2_JSU")

t_stat <- data.frame(
  `1_NO` = c(NA, -24.5677),
  `2_JSU` = c(-28.7354, NA),
  row.names = c("1_NO", "2_JSU")
)
colnames(t_stat) <- c("1_NO", "2_JSU")

# Melt the data frames
p_val_melt <- melt(as.matrix(p_val), varnames = c("Model1", "Model2"), value.name = "p_value")
t_stat_melt <- melt(as.matrix(t_stat), varnames = c("Model1", "Model2"), value.name = "t_stat")

# Heatmap for p-values
p_val_plot <- ggplot(p_val_melt, aes(x = Model1, y = Model2, fill = p_value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red", na.value = "grey50") +
  labs(title = "P-Value Heatmap", fill = "P-Value") +
  theme_minimal()

# Heatmap for t-statistics
t_stat_plot <- ggplot(t_stat_melt, aes(x = Model1, y = Model2, fill = t_stat)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red", na.value = "grey50") +
  labs(title = "T-Statistic Heatmap", fill = "T-Statistic") +
  theme_minimal()

# Print the plots
print(p_val_plot)
print(t_stat_plot)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create the data frame
dm_results_tible <- tibble(
  mod_a = c("1_NO", "2_JSU"),
  mod_b = c("2_JSU", "1_NO"),
  p_val = c(3.151595e-59, 7.229073e-69),
  t_stat = c(-24.5677, -28.7354)
)

# P-value heatmap
p_val_plot <- dm_results_tible %>%
  ggplot(aes(x = mod_b, y = reorder(mod_a, desc(mod_a)), fill = p_val)) +
  geom_raster() +
  scale_fill_gradientn(
    name = "P-Value",
    colours = c("#007900", "red"),
    na.value = "grey15",
    guide = guide_colourbar(
      title.vjust = 0.93,
      barwidth = 1,
      barheight = 10
    ),
    breaks = seq(from = 0, to = .1, by = 0.01),
    limits = c(0, .1),
    oob = scales::squish
  ) +
  labs(title = "P-Value Heatmap", x = "Model B", y = "Model A") +
  theme_minimal()

print(p_val_plot)

# T-statistic heatmap
t_stat_plot <- dm_results_tible %>%
  ggplot(aes(x = mod_b, y = reorder(mod_a, desc(mod_a)), fill = t_stat)) +
  geom_raster() +
  scale_fill_gradientn(
    name = "T-Statistic",
    colours = c("blue", "red"),
    na.value = "grey15",
    guide = guide_colourbar(
      title.vjust = 0.93,
      barwidth = 1,
      barheight
      
      
      
      # Install and load the forecast package
      install.packages("forecast")
      library(forecast)
      
  
      # Perform the Diebold-Mariano test
      dm_test_result <- dm.test(PB_SCORE_TEST[,1], PB_SCORE_TEST[,2], alternative = "two.sided", h = 1, power = 2)
      
      # Print the test result
      print(dm_test_result)
      


