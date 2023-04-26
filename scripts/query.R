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

# Random sample of 20%, followed by join to keep Staten Island rows (About 1000 or so)
dt_sliced <- dt_merged[sample(nrow(dt_merged), 0.2 * nrow(dt_merged))] %>%
 dt_merged[Borough == "Staten Island"][., on = .NATURAL]

saveRDS(dt_sliced, "uber-app/app-data/uber-data.rds")

# Assignment details #
# 
# Using the data attached you are going to perform the following analysis and have it be displayed as a shiny application.
# 
#   20% of the grade is graded on the shiny application, form, function 
#   and displaying all of the following information with explanation of each chart. 
# 
#   40% of the grade is completing all of the following charts, and displaying 
#   the legends and colors in detail with good aesthetics and labeling.
# 
#   20% of the grade is the Geo Spatial leaflet application  application 
#   that you will build within the shiny application
# 
#   10% Building a prediction ride model
# 
#   10% Code documentation within GitHub readme. Requirements have changed. 
#   I only want to see code snippets within the readme. 
#   All bar charts and graphs should be in your Shiny Application. Make sure 
#   your file structure is completed accurately and you have your 
#   shiny URL available for me to click and review your website.
# 
# Pivot table to display trips by the hour.  
# 
# Chart that shows Trips by Hour and Month
# 
# Chart that displays Trips Every Hour.
# 
# Plot data by trips taken during every day of the month.
# 
# I should see a table that shows Trips Every Day (Max 31 days in a month 
# so I should see total trips taken each day). 
# 
# Chart by Trips by Day and Month (bar chart with each day of the week, 
# x axis as the month). I need a chart that shows number of trips by month
# 
# Chart Trips by Bases and Month (Base is the X axis and Month is your label)
# 
# Heat Maps
# 
# Heat map that displays by hour and day
# 
# Heat map by month and day
# 
# Heat map by month and week
# 
# Heat map Bases and Day of Week
# 
# Leaflet Shiny Geospatial Map this portion is subject to change. 