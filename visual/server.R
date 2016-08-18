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
  
  
  TERYT <- seq(from=2, to=32, by=2)
  shades <- colorRampPalette(c("white", "red"))(100)
  
  output$text <- renderText({ 
    paste("You have selected: ", input$wybor)
  })
  
  output$map <- renderPlot({ #wojewodztwa
  
    zmienna <- input$wybor
    wyniki <- vector()
    
    for (i in TERYT){
      #get double digit number e.g. 01 instead of 1
      if (i<10) {
        woj<-paste0("0", i, collapse='')
      } else {
        woj<-paste0(i)
      }
      
      suma_zmienna <- unlist(
                      dbGetQuery(con, paste0("
                                  SELECT SUM([", zmienna, "])   
                                  FROM komisje 
                                  WHERE [Kod.wojewodztwo] = '", woj, "'", collapse = "")
                      ))
      
      suma_lacznie <- unlist(
                      dbGetQuery(con, paste0("
                                  SELECT SUM([Sejm.-.Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkie.listy.kandydatów])
                                  FROM komisje
                                  WHERE[Kod.wojewodztwo] = '", woj, "'", collapse = "")
                      ))
      
      wyniki <- append(wyniki, suma_zmienna/suma_lacznie * 100)
      fills <- shades[round(wyniki)]
      
      plot(panstwo)
      for (i in 1:16) { #temporary solution
        
        woj <- TERYT[i]
        #get double digit number e.g. 01 instead of 1
        if (woj<10) {
          woj<-paste0("0", woj, collapse='')
        } else {
          woj<-paste0(woj)
        }
        
        test <- subset(wojewodztwa, code==woj)
        plot(test, col=fills[i], add=TRUE)
      }
      legend('topright', box.lty=0, bty="n", title=zmienna, legend=round(wyniki,2), fill = fills)
      
    }  
  })

  output$mapa2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data=wojewodztwa)
 })
})


dbDisconnect(con)