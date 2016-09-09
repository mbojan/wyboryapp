library(shiny)
library(leaflet)

source("vars.R")

shinyUI(navbarPage("Wybory parlamentarne",
  tabPanel("Mapa",
           
    div(class="outer",
        
      tags$head(includeCSS("styles.css")),
    
      leafletOutput("map", width="100%", height="100%"),
      
      absolutePanel(
        id = "controls", fixed = TRUE, left = 0, width = 320, top = 50, height = "auto",
        
        helpText("Wyniki wyborów parlamentarnych 2011 i 2015."),
        
        selectInput("type",
                    label=h6(strong("Wybierz rodzaj danych.")),
                    choices=list("Podział administracyjny (2011 i 2015)" ,
                                 "Komisje (2015)"),
                    selected = "Podział administracyjny (2011 i 2015)"
        ),
        
        conditionalPanel(condition = "input.type == 'Podział administracyjny (2011 i 2015)' ",
            
            selectInput("given_year",
                        label = h6(strong("Wybierz rok.")), 
                        choices = list("2015",
                                       "2011"),
                        selected = "2015"
            ),
            
            conditionalPanel(condition = "input.given_year == '2015' ",
                             selectInput("given_var_2015",
                                         label = h6(strong("Wybierz zmienną.")), 
                                         choices = vars_2015)
            ),
            
            conditionalPanel(condition = "input.given_year == '2011' ",
                             selectInput("given_var_2011",
                                         label = h6(strong("Wybierz zmienną.")), 
                                         choices = vars_2011)
            ),
            
            selectInput("given_level",
                        label = h6(strong("Wybierz poziom.")), 
                        choices = map_levels,
                        selected = "wojewodztwa"
            )
            
        ), #end of conditional panel
        
        checkboxInput("borders",
                    label = h6(strong("Pokaż granice."))
        )
        
      ) #end absolutePanel
       
    ) #end div "outer"
    
  ) #end tabpanel
  
  # tabPanel("blank",
  #   sidebarLayout(
  #     
  #     sidebarPanel(
  #       helpText("placeholder")
  #     ), #end sidebarpanel
  #     
  #     mainPanel(
  #     ) #end mainpanel
  #     
  #   ) #end sidebar layout
  #   
  # ) #end tabpanel
))