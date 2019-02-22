shinyServer(function(input, output, session) {

  #Geodata
  Stations <- read_csv("isd-history.csv") %>%
    filter(!is.na(LON) & !is.na(LAT)) %>%
    mutate(BEGIN=ymd(BEGIN), END=ymd(END)) %>%
	filter(format(END, "%Y") > 2018) %>%
    st_as_sf(coords=c("LON", "LAT"), crs=4326)
  
  
  ObtainTimeSeries <- function(StationID, Start, End){
  require(dplyr)
  require(lubridate)
  require(stringr)
  
  StationID <- stringr::str_trim(StationID, "both")
  
  Range <- seq(Start, End)
  FL <- list.files(path="NOAA Data/", pattern=paste0(StationID,"-99999-",Range, collapse="|"), full.names=T)
  
  if(length(FL)==0){
  showNotification("These years were not recorded for this station", type="error", duration=15)
    
  } else {
    open.files <- lapply(FL, readr::read_table)
    TS<-do.call("rbind", open.files)
    
    TS %>%
      mutate(TEMP = ifelse(TEMP==9999.9,NA, TEMP),
             DEWP = ifelse(DEWP==9999.9,NA, DEWP),
             SLP = ifelse(SLP==9999.9,NA, SLP),
             STP = ifelse(STP==9999.9,NA, STP),
             VISIB = ifelse(VISIB==999.9,NA, VISIB),
             WDSP = ifelse(WDSP==999.9,NA, WDSP),
             MXSPD = ifelse(MXSPD==999.9,NA, MXSPD),
             GUST = ifelse(GUST==999.9,NA, GUST),
             MAX = ifelse(MAX=="9999.9",NA, MAX),
             MIN = ifelse(MIN=="9999.9",NA, MIN),
             PRCP = ifelse(PRCP=="99.99",NA, PRCP),
             SNDP = ifelse(SNDP==999.9,NA, SNDP)) %>%
      mutate(TEMP = round((TEMP - 32) * (5/9), 2)) %>%
      mutate(MAX = as.numeric(str_replace(MAX, pattern="\\*",replacement="")),
             MIN = as.numeric(str_replace(MIN, pattern="\\*",replacement="")),
             PRCP = as.numeric(str_replace(PRCP, pattern="[:alpha:]",replacement=""))) %>%
      mutate(MAX = round((MAX - 32) * (5/9), 2),
             MIN = round((MIN - 32) * (5/9), 2)) %>%
      mutate(SNDP = round(SNDP * 25.4, 2),
             PRCP = round(PRCP * 25.4, 2)) %>%
      mutate(WDSP = round(WDSP * 0.51444, 2),
             MXSPD = round(MXSPD * 0.51444, 2),
             GUST = round(GUST * 0.51444, 2)) %>%
      dplyr::rename(StationName = `STN---`, Date=YEARMODA, TempC = TEMP,
             TempNRead=X5, DewpNRead=X7, SeaLevPressure = SLP, SeaLevPresNRead = X9, StationPressure = STP, StationPresNRead = X11, VisibNRead = X13,
             WindSpeed_ms = WDSP, WindNRead = X15, Precip_mm = PRCP, MinTempC = MIN, MaxTempC = MAX,
             SnowDepth_mm = SNDP, MaxWindSpeed_ms = MXSPD, MaxGust_ms = GUST) %>%
      mutate(Date = ymd(Date))
  }
}
  
#Show markers only on zoom and extract based on boundign box

  #Start Shiny Server
  ##################################
  options(shiny.sanitize.errors = F)
  options(shiny.maxRequestSize=30*1024^2)
  
  
  #Leaflet
  #################################
  output$mymap <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("OpenStreetMap") 
    
  })
  
  observeEvent(input$mymap_bounds,{
    #browser()
    if(input$mymap_zoom>=9){
      BB = input$mymap_bounds
      BOX <- st_bbox(c(xmin = BB$west, xmax = BB$east, ymax = BB$north, ymin = BB$south), crs = st_crs(4326))


      SubStations <- st_intersection(Stations, st_as_sfc(BOX))
      #browser()

      P <- paste("ID: ", paste0(SubStations$USAF), "<br>",
                 "Name: ", paste0(SubStations$`STATION NAME`),"<br>",
                 "Elevation: ", paste0(SubStations$`ELEV(M)`),"<br>",
                 "Start Records: ", paste0(SubStations$BEGIN),"<br>",
                 "End Records: ", paste0(SubStations$END),"<br>")

      leafletProxy("mymap") %>%
	  clearMarkers() %>%
        clearMarkerClusters() %>%
        addCircleMarkers(data=SubStations["USAF"], popup=P, stroke = F, fillColor="#bb0000", fillOpacity=0.5)#, clusterOptions = markerClusterOptions())
    } else {
      leafletProxy("mymap") %>%
        clearMarkers() %>%
        clearMarkerClusters()
    }
  })
  
  

# Handle Selector Based Events --------------------------------------------
	TS <- eventReactive(input$data,{
	ObtainTimeSeries(StationID=input$StatID, Start=input$Start, End=input$End)
	})

  observeEvent(input$data,{
    
      if(is.data.frame(TS())){
        output$WeatherOUT <- DT::renderDataTable({
      datatable(TS(), options=list(scrollX=T))
    })
      }
  })

  output$down.csv <- downloadHandler(
    filename = function() {
      paste0("Weather_Station",input$StatID,"_Start",input$Start,"_End",input$End,".csv")
    },
    
    content = function(file) {
      write_csv(TS(), file)
    }
  )
  

  

})
