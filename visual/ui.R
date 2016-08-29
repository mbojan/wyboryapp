library(shiny)
library(leaflet)

source("vars.R")


shinyUI(navbarPage("Wybory",
  tabPanel("2015",
    sidebarLayout(
      
      sidebarPanel(
        helpText("Wyniki wyborow parlamentarnych 2015."),
        helpText("UWAGA! Tymczasowo wszystkie wyniki są skalowane, żeby mapa była lepiej widoczna."),
        
        selectInput("given_year",
                    label = h6(strong("Wybierz rok.")), 
                    choices = list("2015",
                                   "2011"),
                    selected = "2015"
                    ),
        
        conditionalPanel(condition = "input.given_year == '2015' ",
                        selectInput("given_var",
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
        
      ), #end sidebarpanel
      
      mainPanel(
        leafletOutput("map", width="100%", height=600)
      ) #end mainpanel

    ) #end sidebar layout
  ), #end tabpanel
  
  tabPanel("blank",
    sidebarLayout(
      
      sidebarPanel(
        helpText("placeholder")
      ), #end sidebarpanel
      
      mainPanel(
      ) #end mainpanel
      
    ) #end sidebar layout
    
  ) #end tabpanel
))