# Load necessary libraries
library(lubridate)
library(dplyr)
df <- LOAD_Dumm %>% select(1,2,3)
fixed <- holidays %>% filter(Fixed == TRUE & Global == TRUE)
#6#24#6
assdays <- holidays %>% filter(Name=="Assumption")
fixed <- rbind(fixed,assdays)%>% arrange(Date)

flex <-  holidays %>% filter(Fixed == FALSE & Global == TRUE & Name != "Assumption")
#It is easy to handle Flex
flex <- flex %>%filter(Date < "2024-07-01")
flex <- flex%>%filter(Date > "2021-01-10")

fakedate <- seq(from = df$DateTime[1], 
                to = as.POSIXct("2024-07-01 00:00:00", tz = "UTC"), 
                by = "hour")
location <- function(x,holidays){
  locs <- c()
  for (i in 1:length(holidays$Date)) {
    locs[i] <- which(x == holidays$Date[i])
  }
  return(locs)
}
locs_flex <- location(fakedate,flex)

zzz <- function(t,data){
  plot(data$DateTime[(t-24):(t+23+24)],data$Load_ACT[(t-24):(t+23+24)],type="l",xlab=weekdays(data$DateTime[t]),ylim=c(15000,36000))
  lines(data$DateTime[(t-24):(t+23+24)],data$Load_ACT[(t-7*24-24):(t-7*24+23+24)],col="red")
  lines(data$DateTime[(t-24):(t+23+24)],data$Load_ACT[(t+7*24-24):(t+7*24+23+24)],col="blue")
}
zzz(locs_flex[5],df)

#We just have flex public as Good Friday

flex_d <- rep(0,length(seq(from = df$DateTime[1], 
                 to = as.POSIXct("2024-07-01 00:00:00", tz = "UTC"), 
                 by = "hour")))
#flex_d <- rep(0,length(df$DateTime))
for (i in locs_flex) {
  flex_d[(i-6):(i+29)] <- 1
}

plot(flex_d)



######################
#Fixed Global Holidays
######################

fixed <- fixed %>% filter(Date > "2020-01-01")%>%filter(Date < "2025-01-01")


locs <- (fixed %>%filter(Name == unique(fixed$Name)[1]))
locs2 <- location(fakedate,locs)

zzz <- function(t,data){
  plot(data$DateTime[(t-24):(t+23+24)],data$Load_ACT[(t-24):(t+23+24)],type="l",xlab=weekdays(data$DateTime[t]),ylim=c(15000,36000))
  lines(data$DateTime[(t-24):(t+23+24)],data$Load_ACT[(t-7*24-24):(t-7*24+23+24)],col="red")
  lines(data$DateTime[(t-24):(t+23+24)],data$Load_ACT[(t+7*24-24):(t+7*24+23+24)],col="blue")
}
zzz(locs2[1],df)


(fixed %>%filter(Name == unique(fixed$Name)[1]))
###Epiphany if it is monday = 0+24+6- Tuesday 6+24+6 # Wednesday# Thursday # Friday# 6+24+6#Saturday 6+24+0

(fixed %>%filter(Name == unique(fixed$Name)[2]))
###Labour Day  if it is monday = 0+24+6- Tuesday 6+24+6 # Wednesday# Thursday # Friday# 6+24+6#Saturday 6+24+0
zzz(locs2[5],df)

#Assumption
(fixed %>%filter(Name == unique(fixed$Name)[3]))

zzz(locs2[4],df)

fixeddates <- (fixed %>% filter(Date > "2020-01-01" & Date < "2025-01-01"))$Date

loc_f_d_g <- c()
for (i in 1:length(fixeddates)) {
  loc_f_d_g[i] <-which(df$DateTime == fixeddates[i]) 
}




fixed_d <- rep(0,length(fakedate))
for (i in loc_f_d_g) {
  if(weekdays(fakedate[i]) =="Monday"){
    fixed_d[i:(i+29)] <- 1
  } else if(weekdays(fakedate[i]) %in% c("Tuesday", "Wednesday", "Thursday","Friday")){
    fixed_d[(i-6):(i+29)] <- 1
    }
  if(weekdays(fakedate[i]) == "Saturday"){
    fixed_d[(i-6):(i+23)] <- 1
  }
}



#####
regional_flex <-  holidays %>% filter(Fixed == FALSE & Global == FALSE)
regional_flex  <- regional_flex %>% filter(Date > "2020-01-01" & Date < "2025-01-01")

unique(regional_flex$Counties)
#Regional data details Source Wikipedia

# Create the data frame
regions_df <- data.frame(
  region = c("Andalusia", "Catalonia", "Community of Madrid", "Valencian Community", 
             "Galicia", "Castile and León", "Basque Country", "Castile-La Mancha", 
             "Canary Islands", "Murcia", "Aragon", "Balearic Islands", 
             "Extremadura", "Asturias", "Navarre", "Cantabria", "La Rioja", 
             "Ceuta", "Melilla"),
  short_code = c("ES-AN", "ES-CT", "ES-MD", "ES-VC", "ES-GA", "ES-CL", "ES-PV", "ES-CM", "ES-CN", "ES-MC", "ES-AR", "ES-IB", 
                 "ES-EX", "ES-AS", "ES-NC", "ES-CB", "ES-RI", "ES-CE", "ES-ML"),
  population_percentage = c(17.88, 16.02, 13.75, 10.85, 5.88, 5.39, 4.64, 4.49, 
                            4.48, 3.12, 2.86, 2.37, 2.34, 2.28, 1.36, 1.26, 
                            0.68, 0.18, 0.17)
)
# Extract region names
region_codes_vector <- unlist(strsplit(regions_df$short_code, ","))
trt <- unique(regional_flex$Counties)
trt1 <- sum(regions_df$population_percentage[-c(2,4,18,19)])#1
trt2 <- sum(regions_df$population_percentage[c(2,12,17,15,4)])#1
trt3 <- sum(regions_df$population_percentage[3])#0
trt4 <- sum(regions_df$population_percentage[8])#0
trt5 <- sum(regions_df$population_percentage[2])#0
trt6 <- sum(regions_df$population_percentage[-c(2,4,7,16,18,19)])#1
trt_values <- c(trt1, trt2, trt3, trt4, trt5, trt6)
reg_perc_flex <- data.frame("regions"=trt,"percantage"=trt_values)


zzz(which(df$DateTime == (regional_flex%>% filter(Counties ==trt[6])%>%select(Date))[[1]][1]),df)
#
#Easter monday 6-24-6

#Maundy Thursday 6-24-12

# Christmas Day 2022-12-26----0-24-6


list_flex_reg <- reg_perc_flex %>% filter(percantage > 30)
###If monday <- 0+24+6 weekdays 6+24+6 friday 6+24+0 saturday 0+24+0 sunday 0 
reg_flex_d <- rep(0,length(seq(from = df$DateTime[1], 
                            to = as.POSIXct("2024-07-01 00:00:00", tz = "UTC"), 
                            by = "hour")))
fakedate <- seq(from = df$DateTime[1], 
                to = as.POSIXct("2024-07-01 00:00:00", tz = "UTC"), 
                by = "hour")

for (i in 1:length(list_flex_reg$regions)) {
  datesx <- (regional_flex %>% filter(Counties ==list_flex_reg$regions[i]))$Date
  regname <- (regional_flex %>% filter(Counties ==list_flex_reg$regions[i]))$Name[1]
  for (z in 1:length(datesx)){
    id <- which(fakedate == datesx[z])
    if(regname == "Maundy Thursday"){
      if (reg_flex_d[id] == 0){
      reg_flex_d[(id-6):(id+35)] <- 1}
    }else if (regname == "Easter Monday"){
      if (reg_flex_d[id] == 0){
        reg_flex_d[(id-6):(id+29)] <- 1} }
  }
}

which(reg_f_d[which(reg_flex_d != 0)] != 0)


##############################
#Combine Flex Global -Regional
##############################

reg_flex_d
flex_d

which(flex_d != 0)

which(reg_flex_d != 0)

reg_flex_d[which(flex_d != 0)] <- 0

Flex_holidays <- flex_d + reg_flex_d
plot(Flex_holidays)

#####################
#Fixed Regional
#####################


regional_fixed <- holidays %>% filter(Fixed == TRUE & Global == FALSE)
regional_fixed <- regional_fixed %>% filter(Date > "2020-01-01" & Date < "2025-01-01")


frt <- unique(regional_fixed$Counties)

frt1 <- sum(regions_df$population_percentage[1]) #1/frt1
frt2 <- sum(regions_df$population_percentage[12])#0
frt3 <- sum(regions_df$population_percentage[c(8,5,7,4)])#0.5
frt4 <- sum(regions_df$population_percentage[6])#0
frt5 <- sum(regions_df$population_percentage[11])#0
frt6 <- sum(regions_df$population_percentage[5])#0
frt7 <- sum(regions_df$population_percentage[9])#0
frt8 <- sum(regions_df$population_percentage[8])#0
frt9 <- sum(regions_df$population_percentage[17])#0
frt10 <- sum(regions_df$population_percentage[10])#0
frt11 <- sum(regions_df$population_percentage[c(2,4)])#1
frt12 <- sum(regions_df$population_percentage[c(5,7)])#
frt13 <- sum(regions_df$population_percentage[16])#0
frt14 <- sum(regions_df$population_percentage[14])#0
frt15 <- sum(regions_df$population_percentage[13])#0
frt16 <- sum(regions_df$population_percentage[2])
frt17 <- sum(regions_df$population_percentage[4])#0
frt18 <- sum(regions_df$population_percentage[c(13,5,3,7,4)])#1
frt19 <- sum(regions_df$population_percentage[c(3,5,7)])#0
frt20 <- sum(regions_df$population_percentage[c(1,11,14,10,6)])#1
frt21 <- sum(regions_df$population_percentage[3])#0
frt22 <- sum(regions_df$population_percentage[c(6,5,7,15)])#0

frt_values <- c(frt1, frt2, frt3, frt4, frt5, frt6, frt7, frt8, frt9, frt10, 
                frt11, frt12, frt13, frt14, frt15, frt16, frt17, frt18, frt19, 
                frt20, frt21, frt22)
reg_perc <- data.frame("regions"=frt,"percantage"=frt_values)

##
kkkk <- which(df$DateTime ==regional_flex$Date[idx_flr][1])
zzz <- function(t,data){
  plot(data$DateTime[(t-24):(t+23+24)],data$Load_ACT[(t-24):(t+23+24)],type="l",xlab=weekdays(data$DateTime[t]),ylim=c(15000,36000))
  lines(data$DateTime[(t-24):(t+23+24)],data$Load_ACT[(t-7*24-24):(t-7*24+23+24)],col="red")
  lines(data$DateTime[(t-24):(t+23+24)],data$Load_ACT[(t+7*24-24):(t+7*24+23+24)],col="blue")
}
zzz(which(df$DateTime ==regional_flex$Date[idx_flr][4]),df)
#1,3,11,12,16,17,18,19,20,21,22
zzz(which(df$DateTime == (regional_fixed%>% filter(Counties ==frt[16])%>%select(Date))[[1]][10]),df)
# give 0.5 if it is regional holidays fixed for the population is more then %20 1,3,11,16,18,19,20,22 
#1 = > .25 
#0.5 =  .15 >x<.25
#0 for rest

#remove the regional holidays. with less than 0.15 of the total population in Spain
list_ofreg <- reg_perc %>% filter(percantage > 14)
###If monday <- 0+24+6 weekdays 6+24+6 friday 6+24+0 saturday 0+24+0 sunday 0 
reg_f_d <- rep(0,length(seq(from = df$DateTime[1], 
                     to = as.POSIXct("2024-07-01 00:00:00", tz = "UTC"), 
                     by = "hour")))
fakedate <- seq(from = df$DateTime[1], 
                to = as.POSIXct("2024-07-01 00:00:00", tz = "UTC"), 
                by = "hour")
for (i in 1:length(list_ofreg$regions)) {
  datesx <- (regional_fixed %>% filter(Counties == list_ofreg$regions[i]))$Date
  coefx <- c()
  if(list_ofreg$percantage[i] > 24){
    coefx <- 0.5
    } else if (list_ofreg$percantage[i] > 14 & list_ofreg$percantage[i] < 25   ) {
    coefx <- .25}
  for (z in 1:length(datesx)){
    id <- which(fakedate == datesx[z])
    if(weekdays(datesx[z]) == "Monday"){
      reg_f_d[id:(id+29)] <- coefx
      }else if (weekdays(datesx[z]) %in% c("Tuesday", "Wednesday", "Thursday")){
      reg_f_d[(id-6):(id+29)] <- coefx }else if (weekdays(datesx[z]) == "Friday" ){
        reg_f_d[(id-6):(id+29)] <- coefx }
    
  }
}


length(fixed_d)
length(reg_f_d)
which(reg_f_d[which(fixed_d != 0)] !=0)

numx <- which(fixed_d != 0)[which(reg_f_d[which(fixed_d != 0)] !=0)]

reg_f_d[numx] <- 0


plot(reg_f_d + fixed_d)

###########################

###########################
df
