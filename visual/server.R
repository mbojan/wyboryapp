library(DBI)
library(RSQLite)
library(sp)
library(leaflet)

shinyServer(function(input, output) {
  
  con <- dbConnect(SQLite(), "./data/wyniki2015.sqlite3")  
  
  gminy <- readRDS("./data/maps/gminy.rds")
  warszawa <- readRDS("./data/maps/warszawa.rds")
  panstwo <- readRDS("./data/maps/panstwo.rds")
  powiaty <- readRDS("./data/maps/powiaty.rds")
  wojewodztwa <- readRDS("./data/maps/woj.rds")
  
  panstwo <- spTransform(panstwo, CRS("+init=epsg:4326"))
  wojewodztwa <- spTransform(wojewodztwa, CRS("+init=epsg:4326"))
  
  shades <- colorRampPalette(c("white", "darkblue"))(100)
  
  output$text <- renderText({ 
    paste("You have selected: ", input$wybor)
  })
  
  output$mapa <- renderLeaflet({
    
    zmienna <- input$wybor
    wyniki <- vector()
    
    for (i in wojewodztwa$code){
      
      suma_zmienna <- unlist(
        dbGetQuery(con, paste0("
                                  SELECT SUM([", zmienna, "])   
                                  FROM komisje 
                                  WHERE [Kod.wojewodztwo] = '", i, "'", collapse = "")
        ))
      
      suma_lacznie <- unlist(
        dbGetQuery(con, paste0("
                                  SELECT SUM([Sejm.-.Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkie.listy.kandydatów])
                                  FROM komisje
                                  WHERE[Kod.wojewodztwo] = '", i, "'", collapse = "")
        ))
      
      wyniki <- append(wyniki, suma_zmienna/suma_lacznie * 100)
    }
    
    kolory <- shades[round(wyniki)]
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data=wojewodztwa, stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor=kolory)
 })
})


dbDisconnect(con)