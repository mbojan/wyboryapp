library(shiny)


shinyUI(fluidPage(
  titlePanel("wybory"),
  sidebarLayout(
    
    sidebarPanel(
      helpText("Wyniki wyborow parlamentarnych 2015."),
      
      selectInput("wybor",
                  label = h6(strong("Wybierz zmienna.")), 
                  choices = list(
                            "Razem.KW.Prawo.i.Sprawiedliwość",
                            "Razem.KW.Platforma.Obywatelska.RP", 
                            "Razem.KW.Razem",
                            "Razem.KW.KORWiN", 
                            "Razem.Komitet.Wyborczy.PSL", 
                            "Razem.KKW.Zjednoczona.Lewica.SLD+TR+PPS+UP+Zieloni",
                            "Razem.KW.Nowoczesna.Ryszarda.Petru" 
                            )
                  )
    ),
    
    
    mainPanel(tabsetPanel(
      
        tabPanel("Mapa",
                textOutput("text"),
                plotOutput("map")
        )
        
    ))
  )
))