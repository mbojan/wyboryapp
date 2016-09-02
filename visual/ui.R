library(shiny)
library(leaflet)

source("vars.R")

shinyUI(navbarPage("Wybory",
  tabPanel("Wybory parlamentarne",
           
    div(class="outer",
        
      tags$head(includeCSS("styles.css")),
    
      leafletOutput("map", width="100%", height="100%"),
      
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
        top = 70, left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
        
        helpText("Wyniki wyborów parlamentarnych 2011 i 2015."),
        
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
        ),
        
        sliderInput("range",
                    label = h6(strong("Przedział wyników w %:")),
                    min = 0,
                    max = 100,
                    step = 0,
                    value = c(0, 100)
        )
        
      ) #end absolutePanel
       
    ) #end div "outer"
    
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