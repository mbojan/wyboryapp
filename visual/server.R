library(DBI)
library(RSQLite)
library(sp)
library(leaflet)

source("global.R")

shinyServer(function(input, output) {
  
  con <- dbConnect(SQLite(), "./data/wyniki2015.sqlite3")  
  
  gminy <- readRDS("./data/maps/gminy.rds")
  warszawa <- readRDS("./data/maps/warszawa.rds")
  panstwo <- readRDS("./data/maps/panstwo.rds")
  powiaty <- readRDS("./data/maps/powiaty.rds")
  wojewodztwa <- readRDS("./data/maps/woj.rds")
  
  output$text <- renderText({ 
    paste("You have selected: ", input$zmienna)
  })
  
  output$text2 <- renderText({ 
    paste("You have selected: ", input$poziom)
  })
  
  output$mapa <- renderLeaflet({
    
    mapa <- get(input$poziom)
    wyniki <- find_results(input$zmienna, input$poziom, mapa$code, con)
    
    if (!all(is.na(wyniki)) && !all(wyniki==0)){
      while (!max(wyniki, na.rm=TRUE)>50){ #temporary
        wyniki <- wyniki*2
      }
    }
    
    draw_map(mapa, wyniki)
    
 })
})


dbDisconnect(con)