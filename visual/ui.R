library(shiny)
library(leaflet)


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
                            "Razem.KWW.„Kukiz'15”",                                                                                                        
                            "Razem.KW.Nowoczesna.Ryszarda.Petru",                                                                                          
                            "Razem.KWW.JOW.Bezpartyjni",                                                                                                   
                            "Razem.KWW.Zbigniewa.Stonogi",                                                                                                 
                            "Razem.KWW.Grzegorza.Brauna.„Szczęść.Boże!”",                                                                                  
                            "Razem.KW.Samoobrona",                                                                                                         
                            "Razem.KWW.Ruch.Społeczny.RP",                                                                                                 
                            "Razem.Komitet.Wyborczy.Kongres.Nowej.Prawicy",                                                                                
                            "Razem.KWW.Obywatele.do.Parlamentu",                                                                                           
                            "Razem.KWW.Mniejszość.Niemiecka",                                                                                              
                            "Razem.KWW.Zjednoczeni.dla.Śląska"
                            )
                  )
    ),
    
    
    mainPanel(tabsetPanel(
      
        tabPanel("Wojewodztwa",
                textOutput("text"),
                leafletOutput("mapa")
        ),
        
        tabPanel("Temp",
                 h6("test_string")
              
        )
        
    ))
  )
))