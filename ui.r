library(leaflet)
library(leaflet.extras)
library(htmltools)
library(sf)
library(dplyr)
library(shinydashboard)
library(shinysky)
library(DT)
library(lubridate)
library(readr)
library(stringr)




header <- dashboardHeader(
  title = "NOAA Query (v0.1)",
  titleWidth = 300
)



sidebar <- dashboardSidebar(
  sidebarMenu(
    id="tab_menu",
    menuItem("Map", tabName = "map", icon = icon("map-marked-alt"), selected = TRUE),
    menuItem("Download", icon = icon("book-open"), tabName = "download")
  ), width=150
)



body <- dashboardBody(
  
  # Content Tab Map Viewer --------------------------------------------------
  
  
  tabItems(
    tabItem(tabName = "map",
            #h2("Dashboard tab content")
            
            fluidRow(
              box(width = 12, solidHeader = TRUE,
                  
                  
                  leafletOutput("mymap", height = 1000)
                  
                  
              )
              
            )
    ),
    
    tabItem(tabName = "download",
            fluidRow(
              box(width = 3,
                  textInput("StatID", "Station ID")),
              box(width = 3,
                  numericInput("Start", "Start", value=2000)),
              box(width = 3,
                  numericInput("End", "End", value=2018)),
              box(width = 3,
                  actionButton("data", "Create Time-Series:"))
            ),
            fluidRow(
              DT::dataTableOutput("WeatherOUT")
            ),
            fluidRow(
              box(width = 3,
                  downloadButton("down.csv", "Download CSV"))
            )
            
            
    )
  ),
  
  
  
  # Content Tab Data Download -------------------------------------------
  
  
  
  
  
  
  busyIndicator(text = "Please Wait...", wait = 1000)
  
)

dashboardPage(
  header,
  sidebar,
  body
  
)


