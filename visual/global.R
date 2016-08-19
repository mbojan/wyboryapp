library(sp)
library(leaflet)

#************************************************************

generate_query <- function(poziom, zmienna, kod){
  
  query <- paste0("SELECT SUM([", zmienna, "])   
                  FROM komisje", collapse='')
  
  if (poziom != "panstwo"){
    
    poziom <- switch(poziom,
              "powiaty" = "Kod.powiat",
              "wojewodztwa" = "Kod.wojewodztwo",
              "gminy" = "TERYT.gminy")
    
    query <- paste0(query, " WHERE [", poziom, "] = '", kod, "'", collapse = '')
  }

  return(query)
}

#************************************************************

find_results <- function(zmienna, poziom, TERYT, con){
  
  wyniki <- vector()
  
  for (i in TERYT){
  
    suma_zmienna <- unlist(dbGetQuery(con, generate_query(poziom, zmienna, i)))
    
    if (is.finite(suma_zmienna)){
      
      suma_lacznie <- unlist(dbGetQuery(con, generate_query(poziom, "Sejm.-.Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkie.listy.kandydatów", i)))
      dod_wynik <- as.integer(suma_zmienna/suma_lacznie * 100)
    } else {
      dod_wynik <- NA
    }
    wyniki <- append(wyniki, dod_wynik)
  }  

  return(wyniki)
}
  
#************************************************************
  
draw_map <- function(mapa, wyniki){
  
  shades <- colorRampPalette(c("white", "darkblue"))(100)
  
  kolory <- vector()
  
  ind_na <- which(is.na(wyniki))
  ind_zero <- which(wyniki==0)
  ind_non_zero <- which(wyniki!=0)
  
  kolory[ind_na]  <- "gray30"
  kolory[ind_non_zero] <- shades[round(wyniki[ind_non_zero])]
  kolory[ind_zero] <- shades[1]
  
  leaflet() %>%
    addTiles() %>%
    addPolygons(data=mapa, stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor=kolory)
  
}