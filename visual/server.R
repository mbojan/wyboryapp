require(DBI)
require(RSQLite)
require(maptools) 


shinyServer(function(input, output) {
  
  database_name <- "./data/wyniki2015.sqlite3"  
  
  panstwo <- readShapePoly("./data/adm/Państwo.shp")
  wojewodztwa <- readShapePoly("./data/adm/województwa.shp")
  
  con <- dbConnect(SQLite(), database_name)
  
  TERYT <- c(seq(from=2, to=32, by=2))
  
  output$text <- renderText({ 
    paste("You have selected: ", input$wybor)
  })
  
  output$map <- renderPlot({
  
    zmienna <- input$wybor
    wyniki <- vector()
    
    for (i in TERYT){
      #get double digit number e.g. 01 instead of 1
      if (i<10) {
        woj<-paste0("0", i, collapse='')
      } else {
        woj<-paste0(i)
      }
      
      suma_zmienna <- sum(dbGetQuery(con, paste0("select \"Wartosc\" 
                                   from komisje 
                                   where Zmienna=\"", zmienna, "\"  
                                   AND 
                                   \"TERYT.Gminy\" LIKE \"", woj, "%\"", collapse = "")))
      
      suma_lacznie <- sum(dbGetQuery(con, paste0("select \"Wartosc\"
                                         from komisje
                                         where Zmienna=\"Sejm.-.Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkie.listy.kandydatów\"
                                         AND
                                         \"TERYT.Gminy\" LIKE \"", woj, "%\"", collapse = "")))
      
      #suma_lacznie should not update each time - temp solution
      
      wyniki <- append(wyniki, suma_zmienna/suma_lacznie * 100)
      
      shades <- colorRampPalette(c("white", "red"))(100)
      fills <- shades[round(wyniki)]
      
      plot(panstwo)
      for (i in 1:16) { #temporary solution
        
        woj <- TERYT[i]
        #get double digit number e.g. 01 instead of 1
        if (woj<10) {
          woj<-paste0("0", woj, collapse='')
        } else {
          woj<-paste0(woj)
        }
        
        test <- subset(wojewodztwa, jpt_kod_je==woj)
        plot(test, col=fills[i], add=TRUE)
      }
      legend('topright', box.lty=0, bty="n", title=zmienna, legend=round(wyniki,2), fill = fills)
      
    }  
    
  })
  
})


dbDisconnect(con)