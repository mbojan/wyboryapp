source("global.R")

warszawa <- readRDS("./data/maps/warszawa.rds")
gminy <- readRDS("./data/maps/gminy.rds")
powiaty <- readRDS("./data/maps/powiaty.rds")
wojewodztwa <- readRDS("./data/maps/wojewodztwa.rds")
panstwo <- readRDS("./data/maps/panstwo.rds")

coords_2015 <- readRDS("./data/coords_2015.rds")

shinyServer(function(input, output) {
  
  con <- dbConnect(SQLite(), "./data/shiny_db.sqlite3")
  
  output$map <- renderLeaflet({
    
    given_var <- switch(input$given_year,
                  "2015" = input$given_var_2015,
                  "2011" = input$given_var_2011)
    
    color <- switch(given_var,
                    "KW.Platforma.Obywatelska.RP" = "darkorange",
                    "KW.Ruch.Palikota" = "maroon4",
                    "KW.Razem" = "maroon4",
                    "Komitet.Wyborczy.PSL" = "darkgreen",
                    "KW.Polskie.Stronnictwo.Ludowe" = "darkgreen",
                    "KW.Sojusz.Lewicy.Demokratycznej" = "red",
                    "KKW.Zjednoczona.Lewica.SLD+TR+PPS+UP+Zieloni" = "red",
                    "KW.Nowoczesna.Ryszarda.Petru" = "royalblue3",
                    "darkblue")
    
    map <- get(input$given_level)
    
    percent_scores <- reactive({
      find_results(given_var, input$given_level, map$code, con, input$given_year)
    })
    
    draw_map(map, percent_scores(), input$given_level, min=input$range[1], max=input$range[2], color, coords_2015)
    
 })
})
