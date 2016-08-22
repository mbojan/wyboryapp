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

sum_warsaw <- function(zmienna, con){ #Warsaw is the only city for which the district code is specified in the data (and cumulative score isn't stored)
  
  suma_zmienna <- 0
  suma_lacznie <- 0
  NA_found <- FALSE
  
  for (i in 146502:146519){
    kod <- as.character(i)
    
    temp <- unlist(dbGetQuery(con, generate_query("gminy", zmienna, kod)))
    
    if (!NA_found){
      if (is.finite(temp)){
        suma_zmienna <- suma_zmienna + temp
        suma_lacznie <- suma_lacznie + unlist(dbGetQuery(con, generate_query("gminy", "Sejm.-.Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkie.listy.kandydatów", kod)))
      } else {
        NA_found <- TRUE
      }
    }
    
  }
  
  if (NA_found){
    wynik <- NA
  } else {
    wynik <- as.integer(suma_zmienna/suma_lacznie * 100)
  }
  
  return(wynik)
}

#************************************************************

find_results <- function(zmienna, poziom, TERYT, con){
  
  wyniki <- vector()
  
  if (poziom=="warszawa"){
    poziom <- "gminy"
  }
  
  for (i in TERYT){
    
    if (poziom=="gminy" && i=="146501"){
      wyniki <- append(wyniki, sum_warsaw(zmienna, con)) #sums all Warsaw district scores from the db
    } else {
      
      suma_zmienna <- unlist(dbGetQuery(con, generate_query(poziom, zmienna, i)))
      
      if (is.finite(suma_zmienna)){
        
        suma_lacznie <- unlist(dbGetQuery(con, generate_query(poziom, "Sejm.-.Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkie.listy.kandydatów", i)))
        dod_wynik <- as.integer(suma_zmienna/suma_lacznie * 100)
      } else {
        dod_wynik <- NA
      }
      wyniki <- append(wyniki, dod_wynik)
      
    }
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
    addPolygons(data=mapa, stroke = TRUE, weight=1, color="black", fillOpacity = 0.5, smoothFactor = 0.5, fillColor=kolory) %>%
    setView(lng = 19.27, lat = 52.03, zoom = 6)
  
}