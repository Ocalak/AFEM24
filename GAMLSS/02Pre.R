ES_ACT_W <- read.csv("~/Desktop/AFEM24/AFEM24/ES-ACT-W.csv")
ES_Load <- read.csv("~/Desktop/AFEM24/AFEM24/ES-Load.csv")

df <- cbind(ES_Load,ES_ACT_W[,-c(1,2)])
source("DST.R")


time_utc <- df[, "DateTime"] |> as_datetime(tz = "UTC") # UTC time
time_lt <-df[, "DateTime"] |> as_datetime(tz = "CET") # local time (CET/CEST)

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



DST_df <- DST.trafo(
  X = df[,-1],
  Xtime = time_utc,
  Xtz = "CET"
)

unarray <- function(x){
  load_ext_ts <- array(, dim = c(prod(dim(x)[1:2]), dim(x)[3]))
  for (i in 1:dim(x)[3]) {
    # test: i = 1
    load_ext_ts[, i] <- t(x[, , i])
  }
  return(load_ext_ts)
}

df_dst <- unarray(DST_df)
df_dst <- na.omit(df_dst)
summary(df_dst)

colnames(df_dst) <- c("Load_DA","Load_ACT","Temp_ACT","Wind_ACT","Dewp_ACT","Pres_ACT")
df_dst <- df_dst %>% as.data.frame()
df_dst <- df_dst %>%mutate(DateTime = time_fake)




holidays <- read_csv("holidays.csv")
fixed <- holidays %>% filter(Fixed == TRUE & Global == TRUE)
#6#24#6
assdays <- holidays %>% filter(Name=="Assumption")
fixed <- rbind(fixed,assdays)%>% arrange(Date)

flex <-  holidays %>% filter(Fixed == FALSE & Global == TRUE & Name != "Assumption")
#It is easy to handle Flex
flex <- flex %>%filter(Date > "2018-01-01" & Date < "2024-06-01")
location <- function(x,holidays){
  locs <- c()
  for (i in 1:length(holidays$Date)) {
    locs[i] <- which(x == holidays$Date[i])
  }
  return(locs)
}
locs_flex <- location(df_dst$DateTime,flex)

flex_d <- rep(0,length(1:length(df_dst$DateTime)))

#flex_d <- rep(0,length(df$DateTime))
for (i in locs_flex) {
  flex_d[(i-6):(i+29)] <- 1
}


###
fixed <- fixed %>% filter(Date > "2018-01-01" & Date < "2024-06-01")

fixeddates <- (fixed %>% filter(Date > "2018-01-01" & Date < "2024-06-01"))$Date

loc_f_d_g <- c()
for (i in 1:length(fixeddates)) {
  loc_f_d_g[i] <-which(df_dst$DateTime == fixeddates[i]) 
}




fixed_d <- rep(0,length(df_dst$DateTime))
for (i in loc_f_d_g) {
  if(weekdays(df_dst$DateTime[i]) =="Monday"){
    fixed_d[(i):(i+29)] <- 1
  } else if(weekdays(df_dst$DateTime[i]) %in% c("Tuesday", "Wednesday", "Thursday","Friday")){
    fixed_d[(i-6):(i+29)] <- 1
  }else if (weekdays(df_dst$DateTime[i]) == "Saturday"){
    fixed_d[(i-6):(i+23)] <- 1
  }
}



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

##############################
#I have to inclided Navidad additionally.
flex_regional <- holidays %>% filter(Global == FALSE & Fixed == FALSE & Date > "2017-12-01" & Date < "2024-07-01" &LocalName != "Navidad" & LocalName != "Lunes de Pascua Granada")

flex_regional$Counties

#"ES-AN,ES-AR,ES-CL,ES-CM,ES-CN,ES-EX,ES-GA,ES-IB,ES-RI,ES-MD,ES-MC,ES-NC,ES-AS,ES-PV,ES-CB" 0.70
#"ES-CT,ES-IB,ES-RI,ES-NC,ES-PV,ES-VC"   0.28
#"ES-MD" 0.1375
#"ES-CM" 0.5
flex_regional <- flex_regional%>%mutate(coef = rep(c(0.7,0.28,0.1375,0.05),7))

flex_reg_d <- rep(0,nrow(df_dst))
for (i in 1:nrow(flex_regional)) {
  tidx <- which(df_dst$DateTime == flex_regional$Date[i])
  flex_reg_d[(tidx-6):(tidx+29)] <- flex_regional$coef[i]
}
flex_reg_d <- ifelse(flex_reg_d > 0.1,1,0)
unique(flex_reg_d)

###
fixed_regional <- holidays %>% filter(Global == FALSE & Fixed == TRUE & Date > "2017-12-30" & Date < "2024-06-02")




a <- regions_df$population_percentage
coef_x <- data.frame(unique(fixed_regional$Counties),"coef" = c(a[2],a[1],a[12],sum(a[c(4,5,7)]),a[6],a[11],a[5],
                                                                a[9],a[8],a[17],a[10],sum(a[c(2,4)]),a[16],a[14],
                                                                a[13],a[4],sum(a[c(5,7)]),sum(a[c(8,4,5,7)]),sum(a[c(8,4,5,7,3)]),
                                                                sum(a[c(5,4,7)]),sum(a[c(1,11,14,6,10)]),a[3],sum(a[c(6,5,15,7)]))
)

fixed_reg_d <- rep(0,nrow(lassodf))

for (i in 1:nrow(fixed_regional)) {
  if(weekdays(fixed_regional$Date[i]) == "Sunday"){
    idx <- which(df_dst$DateTime == fixed_regional$Date[i])
    fixed_reg_d[idx] <- 0
  }else if (weekdays(fixed_regional$Date[i]) == "Saturday"){
    idx <- which(df_dst$DateTime == fixed_regional$Date[i])   
    coefidx <- which(coef_x$unique.fixed_regional.Counties. == fixed_regional$Counties[i])
    fixed_reg_d[(idx-6):(idx+23)] <- coef_x$coef[coefidx] /100
  } else if (weekdays(fixed_regional$Date[i]) == "Monday"){
    idx <- which(df_dst$DateTime == fixed_regional$Date[i])
    coefidx <- which(coef_x$unique.fixed_regional.Counties. == fixed_regional$Counties[i])
    fixed_reg_d[(idx):(idx+29)] <- coef_x$coef[coefidx] /100
  }else{
    idx <- which(df_dst$DateTime == fixed_regional$Date[i])
    coefidx <- which(coef_x$unique.fixed_regional.Counties. == fixed_regional$Counties[i])
    fixed_reg_d[(idx-6):(idx+29)] <- coef_x$coef[coefidx] /100}
}

plot(fixed_reg_d)

fixed_reg_d <- ifelse(fixed_reg_d > 0.10,1,0)



#Combina fixed and flexible indiviaulry
which((fixed_reg_d+fixed_d)> 1)
###
HolFix <- fixed_reg_d+fixed_d
##
HolFix[which((HolFix) > 1)]
##

which((flex_reg_d+flex_d)> 1)
HolFlx <- flex_reg_d+flex_d
###
HolFlx[which(HolFlx>1)] <- 1



#only for 2019 we take 22
#Create dummy for 24Dec to 6 Jan takes 1 rest zero

#DAtes 2019-2020-2021-2022-2023-2024
s_dates <- c("2018-12-24 00:00:00","2019-12-24 00:00:00","2020-12-24 00:00:00","2021-12-24 00:00:00","2022-12-24 00:00:00","2023-12-24 00:00:00")
e_dates <- c("2019-01-05 23:00:00","2020-01-05 23:00:00","2021-01-05 23:00:00","2022-01-05 23:00:00","2023-01-05 23:00:00","2024-01-05 23:00:00")


Xhol <- rep(0,nrow(df_dst))

which(df_dst$DateTime == "2023-12-22 00:00:00")

Xhol[(8352+168):(8711+168)] <- 1

Xhol[(17112+168):(17112+359+168)] <- 1

Xhol[(25896+168):(25896+168+359)] <- 1

Xhol[(34656+168):(34656+168+359)] <- 1

Xhol[(43416+168):(43416+168+359)] <- 1

Xhol[(52176+168):(52176+168+359)] <- 1



library(glmnet)

library(zoo)



which(yday(df_dst$DateTime) == 366)
which(df_dst$DateTime == "2020-02-28 23:00:00")
#leapyears

df_dst$DateTime[18936:18959]
df_dst$DateTime[54000:54023]


doy <- yday(df_dst$DateTime)

doy[c(18936:18959,54000:54023)] <- 59

which(df_dst$DateTime == "2024-12-31 23:00:00")

doy[18960:26303] <- doy[18960:26303]-1

doy[54024:56255] <- doy[54024:56255]-1

