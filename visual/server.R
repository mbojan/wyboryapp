source("global.R")

warszawa <- readRDS("./data/maps/warszawa.rds")
gminy <- readRDS("./data/maps/gminy.rds")
powiaty <- readRDS("./data/maps/powiaty.rds")
wojewodztwa <- readRDS("./data/maps/wojewodztwa.rds")
panstwo <- readRDS("./data/maps/panstwo.rds")

coords_2015 <- readRDS("./data/coords_2015.rds")

shinyServer(function(input, output) {
  
  con <- dbConnect(SQLite(), "./data/shiny_db.sqlite3")
  
  #map base
  output$map <- renderLeaflet({
    
    view <- c(19.27, 52.03, 7)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = view[1], lat = view[2], zoom = view[3]) 
  })
  
  #add polygons (districts)
  observe({
    
    map_polygons <- get(input$given_level)
    
    given_var <- reactive({
      switch(input$given_year,
             "2015" = input$given_var_2015,
             "2011" = input$given_var_2011)
    })
    
    percent_scores <- reactive({
      find_results(given_var(), input$given_level, map_polygons$code, con, input$given_year)
    })
    
    pal <- colorNumeric("Greens", percent_scores())
    
    scores_text <- round(percent_scores(), 2) %>%
      sapply(function(x) paste0(x, "%", collapse='')) %>%
      gsub(pattern="NA%", replacement="brak wyniku")
    
    leafletProxy("map") %>%
      clearShapes() %>%
      removeControl("Legend") %>%
      addPolygons(data=map_polygons,
                  fillOpacity = 0.5, smoothFactor = 0.5,
                  stroke=input$borders, weight=0.5, color="black", 
                  fillColor = ~pal(percent_scores()),
                  label = unname(mapply(function(x, y) {HTML(sprintf("%s<br>%s", htmlEscape(x), htmlEscape(y)))},
                                        map_polygons$name, scores_text, SIMPLIFY = F))) %>%
      addLegend(position="bottomleft", pal=pal, values = percent_scores(), na.label="Brak wyniku", layerId = "Legend")
  })
  
  
  #switch view to Warsaw
  observe({
    if (input$given_level=="warszawa"){
      view <- c(21.05, 52.24, 11)
      leafletProxy("map") %>%
        setView(lng = view[1], lat = view[2], zoom = view[3])
    }
  })
  
  observe({
    if (input$type == "Komisje (2015)"){
      leafletProxy("map") %>%
        clearShapes() %>%
        removeControl("Legend") %>%
        addMarkers(label=unname(mapply(function(x, y, z)
          {HTML(sprintf("%s<br>Numer obwodu: %i<br>Nazwa komisji: %s", htmlEscape(x), htmlEscape(y), htmlEscape(z)))},
          coords_2015$Gmina, coords_2015$Numer.obwodu, coords_2015$Nazwa.komisji, SIMPLIFY = F)),
          lng=coords_2015$x, lat=coords_2015$y, clusterOptions = markerClusterOptions())
    }
  })
  
})