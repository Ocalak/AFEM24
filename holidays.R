# Load necessary libraries
library(lubridate)
library(dplyr)
holidays_2000_2050
holidays <- holidays_2000_2050 %>% filter(CountryCode == "ES")
fixed <- holidays %>% filter(Fixed == TRUE & Global == TRUE)
#6#24#6
assdays <- holidays %>% filter(Name=="Assumption")
fixed <- rbind(fixed,assdays)%>% arrange(Date)

flex <-  holidays %>% filter(Fixed == FALSE & Global == TRUE & Name != "Assumption")
#It is easy to handle Flex
flex <- flex %>%filter(Date < "2024-07-01")
flex <- flex%>%filter(Date > "2021-01-10")

location <- function(data,holidays){
  locs <- c()
  for (i in 1:length(holidays$Date)) {
    locs[i] <- which(data$DateTime == holidays$Date[i])
  }
  return(locs)
}
locs_flex <- location(all_DF,flex)
zzz <- function(t){
plot(all_DF$DateTime[(t-24):(t+23+24)],all_DF$Load_ACT[(t-24):(t+23+24)],type="l",ylim=c(40000,75000))
lines(all_DF$DateTime[(t-24):(t+23+24)],all_DF$Load_ACT[(t-7*24-24):(t-7*24+23+24)],col="red")
lines(all_DF$DateTime[(t-24):(t+23+24)],all_DF$Load_ACT[(t+7*24-24):(t+7*24+23+24)],col="blue")
}
zzz(locs_flex[3])

flex_d <- rep(0,length(all_DF$DateTime))
for (i in locs_flex) {
  flex_d[(i-12):(i+12)] <- 1
}
####
fixed <- fixed %>% filter(Date > "2020-01-01")%>%filter(Date < "2025-01-01")
fixed %>%filter(Name == unique(fixed$Name)[9])


locs <- (fixed %>%filter(Name == unique(fixed$Name)[2]))
locs2 <- location(all_DF,locs)

zzz <- function(t){
  plot(all_DF$DateTime[(t-24):(t+23+24)],all_DF$Load_ACT[(t-24):(t+23+24)],type="l",xlab=weekdays(all_DF$DateTime[t]),ylim=c(30000,55000))
  lines(all_DF$DateTime[(t-24):(t+23+24)],all_DF$Load_ACT[(t-7*24-24):(t-7*24+23+24)],col="red")
  lines(all_DF$DateTime[(t-24):(t+23+24)],all_DF$Load_ACT[(t+7*24-24):(t+7*24+23+24)],col="blue")
}
zzz(locs2[4])


####
regional_fixed
regional_flex



  }
  
  basis <- coefficients
  return(basis)
}

# Assume 'high_level_load_target' and 'actual_load_target' are vectors of hourly mean loads
# Assume 'timestamp' is a vector of POSIXct timestamps

# Create the basis functions
flex_basis <- create_flex_basis(load_data$timestamp, holidays)
fix_basis <- create_fix_basis(load_data$timestamp, holidays, actual_load_target, high_level_load_target)

# Combine the basis functions into the load data
load_data <- load_data %>%
  mutate(flex_basis = flex_basis,
         fix_basis = fix_basis)


