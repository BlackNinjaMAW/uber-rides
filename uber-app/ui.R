library(shiny)
library(leaflet)
library(lubridate)

fluidPage(

  titlePanel("Uber Rides Data", "Uber Rides Shiny App"),
    
  tags$head(
    # Dark Mode!
    tags$link(rel = "stylesheet", type = "text/css", href = "darkly.min.css"),
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Common input controls
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

      includeMarkdown("app-data/description.md")
    , width = 3),
    mainPanel(
      tabsetPanel(
        tabPanel("Heat Maps",
                 fluidRow(
                   column(width = 6,
                          plotOutput("monthHourHeat")
                   ),
                   column(width = 6,
                          plotOutput("monthDayHeat")
                   ),
                   column(width = 6,
                          plotOutput("monthWdayHeat")
                   ),
                   column(width = 6,
                          plotOutput("baseWdayHeat")
                   )
                 )     
        ),
        tabPanel("Leaflet",
                 uiOutput(outputId = "syncedMaps")
        ),
        tabPanel("Charts",
                 plotOutput("hourChart"),
                 plotOutput("monthHourChart")
        ),
        tabPanel("Tables",
                 fluidRow(
                   column(width = 8,
                          dataTableOutput("monthTable")
                          ),
                   column(width = 4,
                          dataTableOutput("hourTable")
                   )
                 )
        )
      )
      , width = 9)
  )
)
