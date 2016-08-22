library(DBI)
library(RSQLite)
library(sp)
library(leaflet)

source("global.R")

shinyServer(function(input, output) {
  
  con <- dbConnect(SQLite(), "./data/wyniki2015.sqlite3")  
  
  warszawa <- readRDS("./data/maps/warszawa.rds")
  gminy <- readRDS("./data/maps/gminy.rds")
  powiaty <- readRDS("./data/maps/powiaty.rds")
  wojewodztwa <- readRDS("./data/maps/wojewodztwa.rds")
  panstwo <- readRDS("./data/maps/panstwo.rds")
  
  output$text <- renderText({ 
    paste("You have selected: ", input$zmienna)
  })
  
  output$text2 <- renderText({ 
    paste("You have selected: ", input$poziom)
  })
  
  output$mapa <- renderLeaflet({
    
    mapa <- get(input$poziom)
    wyniki <- find_results(input$zmienna, input$poziom, mapa$code, con)
    
    #scaling - temporary
    if (!all(is.na(wyniki)) && !all(wyniki==0) && input$poziom!="panstwo"){
      while (!max(wyniki, na.rm=TRUE)>50){
        wyniki <- wyniki*2
      }
    }
    
    draw_map(mapa, wyniki)
    
 })
})


dbDisconnect(con)