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
    
    given_var <- switch(input$given_year,
                  "2015" = input$given_var_2015,
                  "2011" = input$given_var_2011)
    
    #TO DO: standarize variable names
    color <- switch(given_var,
                    "KW.Platforma.Obywatelska.RP" = "orange",
                    "Razem.KW.Platforma.Obywatelska.RP" = "orange",
                    "KW.Prawo.i.Sprawiedliwość" = "darkblue",
                    "Razem.KW.Prawo.i.Sprawiedliwość" = "darkblue",
                    "KW.Ruch.Palikota" = "darkorange",
                    "Razem.Komitet.Wyborczy.PSL" = "darkgreen",
                    "KW.Polskie.Stronnictwo.Ludowe" = "darkgreen",
                    "KW.Sojusz.Lewicy.Demokratycznej" = "red",
                    "purple")
    
    map <- get(input$given_level)
    
    percent_scores <- find_results(given_var, input$given_level, map$code, con, input$given_year)
    
    draw_map(map, percent_scores, input$given_level, min=input$range[1], max=input$range[2], color)
    
 })
})

