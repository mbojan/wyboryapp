library(shiny)
library(leaflet)


shinyUI(fluidPage(
  titlePanel("wybory"),
  sidebarLayout(
    
    sidebarPanel(
      helpText("Wyniki wyborow parlamentarnych 2015."),
      helpText("UWAGA! Tymczasowo wszystkie wyniki są skalowane, żeby mapa była lepiej widoczna."),
      
      selectInput("given_var",
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
                            ),
                  selected = "Razem.KW.Prawo.i.Sprawiedliwość"
                  ),
      
      selectInput("given_level",
                  label = h6(strong("Wybierz poziom.")), 
                  choices = list(
                    "panstwo",                                               
                    "wojewodztwa",                                                                                           
                    "powiaty",
                    "gminy",
                    "warszawa"
                   ),
                  selected="wojewodztwa"
       )
      
    ),
    
    mainPanel(tabsetPanel(
      
        tabPanel("Mapa",
                textOutput("text"),
                textOutput("text2"),
                leafletOutput("map", width="100%", height=600)
        ),
        
        tabPanel("Temp",
                 h6("test_string")
              
        )
        
    ))
  )
))