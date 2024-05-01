## Code Snipet
# needs meteostat_stations.csv and worldcities.csv

## Load credentials (db_user, db_pw)
## %%
db_user <- "xxxx"
db_password <- "xxxx"
## %%

## Load packages
## %%
library(dplyr)
library(tidyr)
if (!"RMySQL" %in% rownames(installed.packages())) {
  stop("Rymsql is needed.")
}
library(dbx) # Needs RMySQL as well...
library(purrr)
library(DBI)
library(readr)
library(RMySQL)
## %%

## Load data (actual and day-ahead) for e.g. FR (France) from 2015 until now
# =================================================================================================================
# %%
# Establish a connection to the database
db <- dbxConnect(
  adapter = "mysql",
  host = "132.252.60.112",
  port = 3306,
  dbname = "ENTSOE",
  user = db_user,
  password = db_password
)

# Show the list of tables in db
dbListTables(db)

# Obtain specification table
spec <- tbl(db, "spec") %>% collect()

# Get an overview
glimpse(spec)

unique(spec$Name) # We need "Load" here...
unique(spec$Type) # "DayAhead" and "Actual" ...
unique(spec$MapCode) # We want "FR" here...
unique(spec$MapTypeCode) # We take "CTY" here ...

# Lets narrow down the spec table to get the targetTimeSeriesID's
targets <- spec %>%
  filter(
    Name == "Load",
    Type %in% c("DayAhead", "Actual"),
    MapCode == "ES",
    MapTypeCode == "CTY",
  ) %>%
  # Remove empty columns
  select_if(function(x) !(all(is.na(x)) | all(x == "")))


# Obtain (a connection to) the forecasts table
values <- tbl(db, "vals")

glimpse(values)

# Get the actual data
data <- v-alues %>%
  # !! (bang-bang) to unquote the expression
  filter(TimeSeriesID %in% !!targets$TimeSeriesID) %>%
  collect() %>%
  left_join(spec, by = "TimeSeriesID") %>%
  # Filter the joined data to keep only from 2015+
  filter(
    lubridate::year(DateTime) >= 2015
  ) %>%
  # DateTime in UTC
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M", tz = "UTC")) %>%
  # Select the cols of interest
  select(DateTime, Type, Value, MapCode, Name) %>%
  arrange(DateTime) %>%
  pivot_wider(names_from = c(MapCode, Name, Type), values_from = Value)

# Use show_query() to check how the SQL query will look
values %>%
  filter(TimeSeriesID %in% !!targets$TimeSeriesID) %>%
  filter(
    lubridate::year(DateTime) >= 2015
  ) %>%
  show_query()

# Close the connection
dbDisconnect(db)
# %%
# =================================================================================================================


## Weather data for e.g. ES  from 2015 until now
# =================================================================================================================
# %%
# Establish a connection to the database
db <- dbxConnect(
  adapter = "mysql",
  host = "132.252.60.112",
  port = 3306,
  dbname = "DWD_MOSMIX",
  user = db_user,
  password = db_password
)

# Show the list of tables in db
dbListTables(db)

# Get stations from meteostat
meteostat_stations <- read_csv("meteostat_stations.csv")

# Get stations from DWD
stations_dwd <- tbl(db, "locations") %>%
  collect()

# Stations / locations for which we have forecasts and actuals
locations <- meteostat_stations %>%
  select("wmo") %>%
  inner_join(
    stations_dwd,
    by = c("wmo" = "stationid")
  ) %>%
  select(
    "stationid" = "wmo", "stationname",
    "latitude", "longitude", "height"
  )

# Get worldcities
worldcities <- read_csv("worldcities.csv")

n_largest <- 2 # for simplification use mean of Temp in n_largest largest cities

target_cities <- worldcities %>%
  filter(iso2 == "ES") %>%
  group_by(iso2) %>% # If more countries considered
  slice_max(order_by = population, n = n_largest) %>%
  split(.$iso2) # Order changes to alphabetical therefore we sorted above ...


# Find target stations by L2-distance to chosen cities
target_stations <- purrr::map(target_cities, .f = function(x) {
  LOCID <- numeric(nrow(x))
  for (i.x in 1:nrow(x)) { ## to loop across target_cities
    L2toCity <- (locations$longitude - x$lng[i.x])^2 +
      (locations$latitude - x$lat[i.x])^2
    LOCID[i.x] <- locations$stationid[which.min(L2toCity)]
  }
  return(LOCID)
})

# Get weather actuals
meteostat_utc <- tbl(db, "meteostat_utc")

# Overview of table
glimpse(meteostat_utc)

# Get temperature (temp) and wind speed (wspd)
# check https://dev.meteostat.net/bulk/hourly.html#endpoints
# for information on meteostat variable names
weather_actuals <- meteostat_utc %>%
  select(
    stationid, year, month, day, hour, temp, wspd
  ) %>%
  filter(
    stationid %in% !!target_stations[["ES"]]
  ) %>%
  collect() %>%
  # Add leading zeros and convert to POSIXct
  mutate(
    month = sprintf("%02d", month),
    day = sprintf("%02d", day),
    hour = sprintf("%02d", hour)
  ) %>%
  unite("origin", c("year", "month", "day", "hour")) %>%
  mutate(
    origin = anytime::anytime(origin, tz = "UTC", asUTC = TRUE)
  ) %>%
  group_by(origin) %>%
  summarize(
    temp_actual = mean(temp, na.rm = TRUE),
    wspd_actual = mean(wspd, na.rm = TRUE)
  ) %>%
  select("DateTime" = origin, temp_actual,wspd_actual
  ) %>%
  filter(
    lubridate::year(DateTime) >= 2015
  )


# Get weather forecasts for 240 hours

forecasts_utc <- tbl(db, "mosmix_s_utc")

# Overview of table
glimpse(forecasts_utc)

# Get temperature (T5cm) and wind speed (FF)
# check mosmix_elements.xlsx for information on variable names
weather_forecasts <- forecasts_utc %>%
  select(
    stationid,
    oyear, omonth, oday, ohour,
    horizon,
    fyear, fmonth, fday, fhour,
    FF,
    TTT
  ) %>%
  filter(
    stationid %in% !!target_stations[["ES"]],
  ) %>%
  collect() %>%
  # Convert origin and forecast time to POSIXct format
  mutate(
    omonth = sprintf("%02d", omonth),
    oday = sprintf("%02d", oday),
    ohour = sprintf("%02d", ohour),
    fmonth = sprintf("%02d", fmonth),
    fday = sprintf("%02d", fday),
    fhour = sprintf("%02d", fhour)
  ) %>%
  unite("origin", c("oyear", "omonth", "oday", "ohour")) %>%
  unite("forecast", c("fyear", "fmonth", "fday", "fhour")) %>%
  mutate(
    origin = anytime::anytime(origin, tz = "UTC", asUTC = TRUE),
    forecast = anytime::anytime(forecast, tz = "UTC", asUTC = TRUE)
  )

# Check the available forecasting horizons
weather_forecasts %>% distinct(horizon)

weather_forecasts <- weather_forecasts %>%
  group_by(origin, horizon,forecast) %>%
  summarize(
    temp_forecast = mean(TTT - 273.15, na.rm = TRUE), # Convert Kelvin to Celsius
    wspd_forecast = mean(FF, na.rm = TRUE)   
  ) %>%
  select("DateTime" = origin,horizon,forecast, temp_forecast,wspd_forecast) %>%
  filter(lubridate::year(DateTime) >= 2015)

# Close the connection
dbDisconnect(db)
# %%


install.packages("FAIRmaterials")
library(FAIRmaterials)

#First Issue: replace Nulls with NA
xs <- replace_null_recursive(data)
####
#Unlist the load forecasts and actuals
#xsdata <- data_frame("DateTime"=xs$DateTime,"Load_DA"=unlist(xs$ES_Load_DayAhead),
                  # "Load_Act"=unlist(xs$ES_Load_Actual))



#Second issue: Some lists are containing 2 obs
table(sapply(xs$ES_Load_DayAhead,length))
table(sapply(xs$ES_Load_Actual,length))


which(sapply(xs$ES_Load_Actual,length)==2)
which(sapply(xs$ES_Load_DayAhead,length)==2)


idx <- which(sapply(xs$ES_Load_Actual,length)==2)
which(sapply(xs$ES_Load_DayAhead,length)==2)

#Fix the list which contains two obs. 
for (i in idx) {
  xs$ES_Load_DayAhead[i] <- mean(unlist(xs$ES_Load_DayAhead[i]))
  xs$ES_Load_Actual[i] <- mean(unlist(xs$ES_Load_Actual[64777]))
  
  }




data_df <- data.frame("DateTime"=xs$DateTime,"Load_DA"=xs$ES_Load_DayAhead,"Load_Act"=xs$ES_Load_Actual)




#3rd issue: The frequency is changing on the year 2021 15mins. Before that it was hourly frequency.


                                                                                                      ))






