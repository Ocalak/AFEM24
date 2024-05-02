#
holiday_dates <- holidays_2000_2050
holiday_dates$Date <- as.POSIXct(holiday_dates$Date)

holiday_dates <- holiday_dates %>% 
  filter(Date > as.Date("2014-12-26", tz = "UTC"))

holiday_dates <- holiday_dates %>%
  filter(Date < as.Date("2024-08-01", tz = "UTC"))


holiday_dates <- holiday_dates %>% filter(CountryCode == "ES")
unique(holiday_dates$Name)

#Index  no of public holidays at all regions 
idx_h <- which(is.na(holiday_dates$Counties)== TRUE)

for (i in idx_h) {
  holiday_dates$Counties[i] <- "All"
}

2015-02-28 00:00:00 Día de Andalucía                  Day of Andalucía    

2016-02-28
which(dst_data$DateTime == as.Date("2015-02-28 00:00:00"))#1392'Saturday
which(dst_data$DateTime == as.Date("2015-02-21 00:00:00"))#

which(dst_data$DateTime == as.Date("2016-02-28 00:00:00"))#10152#Sunday
which(dst_data$DateTime == as.Date("2016-02-21 00:00:00"))#

which(dst_data$DateTime == as.Date("2017-02-28 00:00:00"))#10152 Tuesday
which(dst_data$DateTime == as.Date("2017-02-21 00:00:00"))# Tuesday



plot(0:23,dst_data[1392:1415,3],type="l",ylim=c(18000,30000))
lines(0:23,dst_data[10152:10175,3],col="red",type="l")
lines(0:23,dst_data[18936:18959,3],col="blue",type="l")
#A week befores
lines(0:23,dst_data[1224:1247,3],col="green",type="l")
lines(0:23,dst_data[9984:10007,3],col="orange",type="l")
lines(0:23,dst_data[18768:18791,3],col="purple",type="l")
legend("topright",                      # Position of the legend in the plot
       legend = c("28FEB15 Public Holiday- Saturday",       
                  "28FEB16 Public Holiday- Sunday",         
                  "28FEB17 Public Holiday- Tuesday",        
                  "A week before of 28FEB15- Saturday",
                  "A week before of 28FEB16- Sunday",#
                  "A week before of 28FEB17- Tuesday"),# 
       col = c("black", "red", "blue", "green", "orange", "purple"),  
       lty = 1,                          
       cex = 0.8) 

