library(data.table)
library(KernSmooth)
library(magrittr)
library(leafsync)
library(mapview) 
library(leaflet)
library(ggplot2)
library(raster)
library(shiny)

hour_levels <- c(
  "1 AM",  "2 AM", "3 AM", "4 AM",  
  "5 AM",  "6 AM",  "7 AM", "8 AM",  
  "9 AM",  "10 AM", "11 AM", "12 PM", 
  "1 PM",  "2 PM",  "3 PM", "4 PM",  
  "5 PM",  "6 PM",  "7 PM", "8 PM",  
  "9 PM",  "10 PM", "11 PM", "12 AM"
)

month_levels <- c(
  "Apr", "May", "Jun", "Jul", "Aug", "Sep"
)

day_levels <- c(
  "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"
)

sky_colors <- c(
  "1 AM"  = "#14192d",
  "2 AM"  = "#15213f",
  "3 AM"  = "#192850",
  "4 AM"  = "#203460",
  "5 AM"  = "#143264",
  "6 AM"  = "#1e518f",
  "7 AM"  = "#3065A5",
  "8 AM"  = "#326BB6",
  "9 AM"  = "#337ec9",
  "10 AM" = "#417DB9",
  "11 AM" = "#4470aa",
  "12 PM" = "#4170a8",
  "1 PM"  = "#3F6BA2",
  "2 PM"  = "#9Cb3cB",
  "3 PM"  = "#89a3bd",
  "4 PM"  = "#7690a6",
  "5 PM"  = "#676785",
  "6 PM"  = "#595067",
  "7 PM"  = "#341c1b",
  "8 PM"  = "#2F141c",
  "9 PM"  = "#2A192b",
  "10 PM" = "#28192b",
  "11 PM" = "#171720",
  "12 AM" = "#151524"
)

dt_all <- readRDS("app-data/uber-data.rds") %>%
  .[, Formatted.Hour := factor(Formatted.Hour, levels = hour_levels)] %>%
  .[, Formatted.Day := factor(Formatted.Day, levels = day_levels)] %>%
  .[, Formatted.Month := factor(Formatted.Month, levels = month_levels)] %>%
  .[, Borough := as.factor(Borough)] %>%
  .[, Base := as.factor(Base)] %>%
  .[, Day := as.factor(Day)]

plot_theme <- ggdark::dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(color = "#F7F7F7"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey40", linewidth = 0.2),
        panel.grid.minor = element_line(color = "grey40", linewidth = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27),
        axis.text = element_text(color = "#F7F7F7"),
        legend.text = element_text(color = "#F7F7F7"),)

# To fit Leaflet; too painful to use SP to calculate it, so done manually
new_york_bounds <- list(
  SW = c(40.496044, -74.255735),
  NE = c(40.915256, -73.700272) 
)



# Define server logic required to draw a histogram
function(input, output, session) {
  
  updateSelectInput(session, "base",
                    label = "Select base to filter by:",
                    choices = c("All", as.character(unique(dt_all[["Base"]]))),
                    selected = "All"
  )
  
  updateSelectInput(session, "borough",
                    label = "Select borough to filter by:",
                    choices = c("All", as.character(unique(dt_all[["Borough"]]))),
                    selected = "All"
  )
  
  dt_filtered <- reactive({
    req(input$base, input$borough)
    
    dt_all %>% 
      .[input$base == "All" | input$base == Base] %>%
      .[input$borough == "All" | input$borough == Borough]
  })

  
  output$synced_maps <- renderUI({
    dt_data <- dt_filtered()

    # Create kernel density output
    kde <- bkde2D(dt_data[, list(Lon, Lat)],
                  bandwidth=c(.0045, .0068), gridsize = c(1000,1000))

    # Create Raster from Kernel Density output
    KernelDensityRaster <- raster(list(x=kde$x1, y=kde$x2, z = kde$fhat))

    # Set low density cells as NA so we can make them transparent with the colorNumeric function
    KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 1)] <- NA

    palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")

    # Leaflet map with markers
    discreteMap <- leaflet() %>% addTiles() %>%
      addRasterImage(KernelDensityRaster,
                     colors = palRaster,
                     opacity = .5) %>%
      fitBounds(
        lng1 = new_york_bounds$SW[2],
        lat1 = new_york_bounds$SW[1],
        lng2 = new_york_bounds$NE[2],
        lat2 = new_york_bounds$NE[1]
      )

    palRaster <- colorBin("Spectral", bins = 7, domain = KernelDensityRaster@data@values, na.color = "transparent")

    # Leaflet map with raster
    binnedMap <- leaflet() %>% addTiles() %>%
      addRasterImage(KernelDensityRaster,
                     colors = palRaster,
                     opacity = .5) %>%
      addLegend(pal = palRaster,
                values = KernelDensityRaster@data@values,
                title = "Kernel Density of Points") %>%
      fitBounds(
        lng1 = new_york_bounds$SW[2],
        lat1 = new_york_bounds$SW[1],
        lng2 = new_york_bounds$NE[2],
        lat2 = new_york_bounds$NE[1]
      )

    sync(discreteMap, binnedMap, ncol = 1)
  })
  
  output$hourChart <- renderCachedPlot({
    dt_hours <- dt_filtered() %>%
      .[, .(Trips = sum(.N)), by = Formatted.Hour]
    
    ggplot(dt_hours, aes(x = Formatted.Hour, y = Trips, fill = Formatted.Hour)) + 
      geom_bar(show.legend = F, position = "dodge", stat = "identity", color = "grey40", linewidth = 0.05) +
      scale_fill_manual(values = sky_colors) +
      labs(title = "# of Trips by Hour", x = "Hour", fill = "Hour") +
      scale_y_continuous(labels = scales::comma) +
      coord_flip() +
      plot_theme
  }, cacheKeyExpr = { list(input$base, input$borough) }, bg="transparent")
  
  output$monthHourChart <- renderCachedPlot({
    dt_month_hours <- dt_filtered() %>%
      .[, .(Trips = sum(.N)), by = c("Formatted.Month", "Formatted.Hour")]
    
    max_bar_april <- dt_month_hours %>%
      .[Formatted.Month == "Apr",] %>%
      .[which.max(Trips[Formatted.Month == "Apr"]),]
    
    ggplot(dt_month_hours, aes(x = Formatted.Month, y = Trips, fill = Formatted.Hour)) + 
      geom_bar(show.legend = T, position = "dodge", stat = "identity", color = "grey40", linewidth = 0.05) +
      scale_fill_manual(values = sky_colors) +
      labs(title = "# of Trips by Month and Hour", x = "Month", fill = "Hour") +
      geom_text(data = max_bar_april, aes(x = as.numeric(Formatted.Month) - 0.45, label = Formatted.Hour, y = Trips + max(dt_month_hours$Trips) * 0.02), size = 3, color = "white") +
      geom_segment(data = max_bar_april,
                   aes(x = as.numeric(Formatted.Month) - 0.5, xend = as.numeric(Formatted.Month) - 0.5,
                       y = 0, yend = Trips),
                   color = "white", linetype = "dashed") +
      guides(fill = guide_legend(nrow = 3, ncol = 8)) +
      plot_theme +
      theme(legend.position = "bottom", legend.key = element_rect(color = "grey40"))
    
  }, cacheKeyExpr = { list(input$base, input$borough) }, bg="transparent")
  
  output$monthHourHeat <- renderCachedPlot({
    dt_month_hours <- dt_filtered() %>%
      .[, .(Trips = sum(.N)), by = c("Formatted.Month", "Formatted.Hour")]

    ggplot(dt_month_hours, aes(x = Formatted.Month, y = Formatted.Hour, fill = Trips)) +
      geom_tile(color = "black") +
      scale_fill_distiller(palette = "Spectral") +
      labs(title = "Heat Map of Uber Rides", subtitle = "Plotted by Hour and Month",
           x = "Month", y = "Hour of the Day", fill = "# of Trips",
           caption = "Each tile represents an hour of a given month, with the fill indicating the\n number of trips scheduled at that hour across all days of the month.") +
      plot_theme +
      theme(legend.position = "right")
  }, cacheKeyExpr = { list(input$base, input$borough) }, bg="transparent")

  output$monthDayHeat <- renderCachedPlot({
    dt_month_days <- dt_filtered() %>%
      .[, .(Trips = sum(.N)), by = c("Formatted.Month", "Day")]

    ggplot(dt_month_days, aes(x = Formatted.Month, y = Day, fill = Trips)) +
      geom_tile(color = "black") +
      scale_fill_distiller(palette = "Spectral") +
      labs(title = "Heat Map of Uber Rides", subtitle = "Plotted by Month and Day",
           x = "Month", y = "Day of the Month", fill = "# of Trips",
           caption = "Each tile represents a single day of a given month, with the fill indicating the\n number of trips scheduled that day.") +
      plot_theme +
      theme(legend.position = "right")
  }, cacheKeyExpr = { list(input$base, input$borough) }, bg="transparent")

  output$monthWdayHeat <- renderCachedPlot({
    dt_month_wdays <- dt_filtered() %>%
      .[, .(Trips = sum(.N)), by = c("Formatted.Month", "Formatted.Day")]

    ggplot(dt_month_wdays, aes(x = Formatted.Month, y = Formatted.Day, fill = Trips)) +
      geom_tile(color = "black") +
      scale_fill_distiller(palette = "Spectral") +
      labs(title = "Heat Map of Uber Rides", subtitle = "Plotted by Month and Day of Week",
           x = "Month", y = "Day of the Week", fill = "# of Trips",
           caption = "Each tile represents a week day of a given month, with the fill indicating the\n number of trips scheduled that day of the week across all weeks of the month.") +
      plot_theme +
      theme(legend.position = "right")
  }, cacheKeyExpr = { list(input$base, input$borough) }, bg="transparent")

  output$baseWdayHeat <- renderCachedPlot({
    dt_base_wdays <- dt_filtered() %>%
      .[, .(Trips = sum(.N)), by = c("Base", "Formatted.Day")]

    ggplot(dt_base_wdays, aes(x = Base, y = Formatted.Day, fill = Trips)) +
      geom_tile(color = "black") +
      scale_fill_distiller(palette = "Spectral") +
      labs(title = "Heat Map of Uber Rides", subtitle = "Plotted by Base and Day of Week",
           x = "Month", y = "Day of the Week", fill = "# of Trips",
           caption = "Each tile represents a week day of service for a given base, with the fill indicating the\n number of trips scheduled that day of the week across all trips for that base.") +
      plot_theme +
      theme(legend.position = "right")
  }, cacheKeyExpr = { list(input$base, input$borough) }, bg="transparent")
}

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
#   All bar charts and graphs should be in your ShinyApplication. Make sure 
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