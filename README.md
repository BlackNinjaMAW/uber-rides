Uber Rides Analysis
=
This repository contains scripts and data for a light analysis of a dataset provided by Uber, containing spatial data of rides in New York City.

Data
-

The data used in this analysis consists of Uber rides data from April to September 2014. The raw data files are located in the `scripts/data/raw-data/` directory. The processed data used in the Shiny app is saved as an RDS file in the `uber-app/app-data/` directory. Additionally, a GeoJSON file containing the boundaries of New York City boroughs is located in the `scripts/data/geospatial-data/` directory.

Scripts
-

The repository contains the following scripts:

### `query.R`

This script loads, cleans, and combines the raw Uber data from multiple CSV files. It then performs a spatial join to associate each point with a borough in NYC. Finally, the script creates additional columns for plotting in the app and saves the resulting data table to an RDS file.

### `server.R`

This script is the server logic for the Shiny app. It contains functions for loading the data, creating kernel density maps, and rendering various charts and heatmaps based on user input.

### `ui.R`

This script implements the plots and maps generated within the server function, specifying the layout of the user interface. The ui is organized as a sidebar layout, with a sidebar containing inputs to filter by, and a main pane containing a tabset.  

Notable code snippets
---------------------

### Loading and merging data (from `query.R`)

```R
dt_apr <- fread("scripts/data/raw-data/uber-raw-data-apr14.csv")
dt_may <- fread("scripts/data/raw-data/uber-raw-data-may14.csv")
dt_jun <- fread("scripts/data/raw-data/uber-raw-data-jun14.csv")
...
dt_all <- merge.data.table(dt_apr, dt_may, all = T) %>%
	merge.data.table(dt_jun, all = T) %>%
  	...
```

### Performing spatial join (from `query.R`)

```R
nyc_boroughs <- st_read("scripts/data/geospatial-data/new-york-boroughs.geojson")

sf_data <- st_as_sf(dt_all, coords = c("Lon", "Lat"), crs = 4326)

sf_data_with_borough <- st_join(sf_data, nyc_boroughs)
```

### Rendering synced maps with density raster (from `server.R`)

```R
output$synced_maps <- renderUI({
	...
	# Create kernel density output
	kde <- bkde2D(dt_data[, list(Lon, Lat)],
		  bandwidth=c(.0045, .0068), gridsize = c(1000,1000))

	# Create Raster from Kernel Density output
	KernelDensityRaster <- raster(list(x=kde$x1, y=kde$x2, z = kde$fhat))

	# Set low density cells as NA so we can make them transparent with the colorNumeric function
	KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 1)] <- NA

	palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")
    	...
	sync(discreteMap, binnedMap, ncol = 1)
})
```

### Creating a heatmap (from `server.R`)

```R
output$monthHourHeat <- renderCachedPlot({
	dt_month_hours <- dt_filtered() %>%
		.[, .(Trips = sum(.N)), by = c("Formatted.Month", "Formatted.Hour")]
	
	ggplot(dt_month_hours, aes(x = Formatted.Month, y = Formatted.Hour, fill = Trips)) +
		geom_tile(color = "black") +
    ...
})
```

### Creating a chart (from `server.R`)

```R
output$hourChart <- renderCachedPlot({
    dt_hours <- dt_filtered() %>%
      .[, .(Trips = sum(.N)), by = Formatted.Hour]
    
    ggplot(dt_hours, aes(x = Formatted.Hour, y = Trips, fill = Formatted.Hour)) + 
      geom_bar(show.legend = F, position = "dodge", stat = "identity", ...) +
      ...
```

### Specify tabset layout (from `ui.R`)

```R
tabsetPanel(
        tabPanel("Heat Maps",
                 fluidRow(
                   column(width = 6,
                          plotOutput("monthHourHeat")
                   ),
                   column(width = 6,
                          plotOutput("monthDayHeat"),
                   ),
                   column(width = 6,
                          plotOutput("monthWdayHeat"),
                   ),
                   column(width = 6,
                          plotOutput("baseWdayHeat")
                   )
                 )     
        ),
        tabPanel("Leaflet",
                 uiOutput(outputId = "synced_maps")
        ),
        tabPanel("Charts",
                 plotOutput("hourChart"),
                 plotOutput("monthHourChart")
        )
      )
```

### Specify input layout (from `ui.R`)

```R
selectInput("base", 
	  label = "Select base to filter by:", 
	  choices = c("All"),
	  selected = "All"
	  ),
selectInput("borough",
	  label = "Select borough to filter by:", 
	  choices = c("All"),
	  selected = "All"
),
```

### Populate inputs (from `server.R`)

```R
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
```
