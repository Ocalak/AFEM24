install.packages("FAIRmaterials")
library(FAIRmaterials)

#First Issue: replace Nulls with NA
xs <- replace_null_recursive(data)
####
#Unlist the load forecasts and actuals
#xsdata <- data_frame("DateTime"=xs$DateTime,"Load_DA"=unlist(xs$ES_Load_DayAhead),
# "Load_Act"=unlist(xs$ES_Load_Actual))


#SEcond issue: Some lists are containing 2 obs
table(sapply(xs$ES_Load_DayAhead,length))
table(sapply(xs$ES_Load_Actual,length))


idx <- which(sapply(xs$ES_Load_Actual,length)==2)
idx2 <- which(sapply(xs$ES_Load_DayAhead,length)==2)

#Fix the list which contains two obs. 
for (i in idx) {
  xs$ES_Load_DayAhead[i] <- mean(unlist(xs$ES_Load_DayAhead[i]))
  xs$ES_Load_Actual[i] <- mean(unlist(xs$ES_Load_Actual[i]))
  
}




data_df <- data.frame("DateTime"=xs$DateTime,"Load_DA"=unlist(xs$ES_Load_DayAhead),"Load_Act"=unlist(xs$ES_Load_Actual))
#write.csv(data_df,"~/Desktop/AFEM24/AFEM24/data_df.csv")

summary(data_df)


#Third issue Frequency about 2021 data is in 15min freq
#We should agregate it to hourly frequncy    ))
library(dplyr)
library(lubridate)
data_df <- data_df %>%
  mutate(DateTime = floor_date(DateTime, "hour")) %>%
  group_by(DateTime) %>%
  summarise(Load_DA = mean(Load_DA, na.rm = TRUE),
            Load_ACT = mean(Load_Act,na.rm = TRUE))


plot(data_df$DateTime)

summary(data_df)
which(is.na(data_df$Load_ACT) == TRUE)

# I replace NA's with last know values for now.
library(zoo)
data_df <- na.locf(data_df)
summary(data_df)
#######
Sys.setlocale(locale = "en_US")
##TEst set should start from "01.12.2023 00:00:00"

data_dfx <- as.data.frame(data_df)

time_utc <- data_dfx[, "DateTime"] |> as_datetime(tz = "UTC") # UTC time
time_lt <- data_dfx[, "DateTime"] |> as_datetime(tz = "CET") # local time (CET/CEST)

which(data_df$DateTime == as_datetime("2023.12.01 00:00:00"))# test data should start form here.


S = 24
start_end_time <- strptime(
  format(c(time_lt[1], time_lt[length(time_lt)]), "%Y-%m-%d %H:%M:%S",
         tz = "CET"
  ),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

time_fake_numeric <- seq(
  from = as.numeric(start_end_time[1]),
  to = as.numeric(start_end_time[length(start_end_time)]),
  by = 24 * 60 * 60 / S
)

time_fake <- as.POSIXct(time_fake_numeric, origin = "1970-01-01", tz = "UTC")

dates <- unique(as.Date(time_fake))

Load_DST_arr <- DST.trafo(
  X = data_dfx[,-1],
  Xtime = time_utc,
  Xtz = "CET"
) # may take 


dst_data <- data.frame("DateTime"=time_fake,"Load_DA"=as.vector(Load_DST_arr[-1,,1])[1:81839],"Load_ACT"=as.vector(Load_DST_arr[-1,,2])[1:81839])

length(time_fake)

#write_csv(dst_data,"~/Desktop/AFEM24/AFEM24/DST_DF.csv")



