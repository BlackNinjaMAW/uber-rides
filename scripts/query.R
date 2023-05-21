library(data.table)
library(tibble)
library(clock)
library(sf)

dt_apr <- fread("scripts/data/raw-data/uber-raw-data-apr14.csv")
dt_may <- fread("scripts/data/raw-data/uber-raw-data-may14.csv")
dt_jun <- fread("scripts/data/raw-data/uber-raw-data-jun14.csv")
dt_jul <- fread("scripts/data/raw-data/uber-raw-data-jul14.csv")
dt_aug <- fread("scripts/data/raw-data/uber-raw-data-aug14.csv")
dt_sep <- fread("scripts/data/raw-data/uber-raw-data-sep14.csv")

# Combine CSVs and format date column
dt_all <- merge.data.table(dt_apr, dt_may, all = T) %>%
  merge.data.table(dt_jun, all = T) %>%
  merge.data.table(dt_jul, all = T) %>%
  merge.data.table(dt_aug, all = T) %>%
  merge.data.table(dt_sep, all = T) %>%
  setnames(old = c("Date/Time"), new = c("Date.Time")) %>%
  .[, Date.Time := as.POSIXct(Date.Time, format = "%m/%d/%Y %H:%M:%OS")]

rm(list = c("dt_apr", "dt_may", "dt_jun", "dt_jul", "dt_aug", "dt_sep"))

# GeoJSON file containing NYC borough boundaries
nyc_boroughs <- st_read("scripts/data/geospatial-data/new-york-boroughs.geojson")

# Create feature object from data
sf_data <- st_as_sf(dt_all, coords = c("Lon", "Lat"), crs = 4326)

# Perform spatial join to associate each point with a borough
sf_data_with_borough <- st_join(sf_data, nyc_boroughs)

# Create data.table from feature object
dt_sf <- as.data.table(sf_data_with_borough)

# Bind boroughs to old data containing Lon and Lat
dt_boroughs <- dt_all %>%
  data.table(Borough = dt_sf$boro_name) %>%
  .[, Borough := ifelse(is.na(Borough), "Greater New York Area", Borough)]
  
# Create new columns for plotting in app
dt_merged <- dt_boroughs %>%
  .[, Month := get_month(Date.Time)] %>%
  .[, Day := get_day(Date.Time)] %>%
  .[, Hour := get_hour(Date.Time)] %>%
  .[, Formatted.Month := month.abb[Month]] %>%
  .[, Formatted.Day := as.character(as_weekday(Date.Time))] %>%
  .[, Formatted.Hour := paste((Hour - 1) %% 12 + 1, ifelse(Hour < 12, "AM", "PM"))] %>%
  .[, c("Month", "Hour", "Date.Time") := NULL]

saveRDS(dt_merged, "uber-app/app-data/uber-data.rds")