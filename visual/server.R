source("global.R")

shinyServer(function(input, output) {
  
  con_2015 <- dbConnect(SQLite(), "./data/wyniki2015.sqlite3")
  con_2011 <- dbConnect(SQLite(), "./data/wyniki2011.sqlite3")  
  
  warszawa <- readRDS("./data/maps/warszawa.rds")
  gminy <- readRDS("./data/maps/gminy.rds")
  powiaty <- readRDS("./data/maps/powiaty.rds")
  wojewodztwa <- readRDS("./data/maps/wojewodztwa.rds")
  panstwo <- readRDS("./data/maps/panstwo.rds")
  
  output$text <- renderText({ 
    paste("You have selected: ", input$given_var)
  })
  
  output$text2 <- renderText({ 
    paste("You have selected: ", input$given_level)
  })
  
  output$map <- renderLeaflet({
    
    con <- switch(input$given_year,
                  "2015" = con_2015,
                  "2011" = con_2011)
    
    var <- switch(input$given_year,
                  "2015" = input$given_var,
                  "2011" = input$given_var_2011)
    
    map <- get(input$given_level)
    percent_scores <- find_results(var, input$given_level, map$code, con, input$given_year)
    
    #scaling - temporary
    #if (!all(is.na(percent_scores)) && !all(percent_scores == 0) && input$given_level != "panstwo"){
    #  while (!max(percent_scores, na.rm=TRUE)>50){
    #    percent_scores <- percent_scores*2
    #  }
    #}
    
    draw_map(map, percent_scores, input$given_level)
    
 })
})

