library(sp)
library(leaflet)

#************************************************************

get_from_db <- function(poziom, zmienna, kod, con){
  
  query <- paste0("SELECT SUM([", zmienna, "]),
                  SUM([Sejm.-.Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkie.listy.kandydatów])
                  FROM komisje", collapse='')
  
  if (poziom != "panstwo"){
    poziom <- switch(poziom,
              "powiaty" = "Kod.powiat",
              "wojewodztwa" = "Kod.wojewodztwo",
              "gminy" = "TERYT.gminy")
    
    if (grepl(pattern = "[%]", x = kod)){
      query <- paste0(query, " WHERE [", poziom, "] LIKE '", kod, "'", collapse = '')
    } else {
      query <- paste0(query, " WHERE [", poziom, "] = '", kod, "'", collapse = '')
    }
    
  }
  
  result <- dbGetQuery(con, query)

  return(result)
}

#************************************************************

find_results <- function(zmienna, poziom, TERYT, con){
  
  wyniki <- vector()
  
  if (poziom == "warszawa"){
    poziom <- "gminy"
  }
  
  for (i in TERYT){
    
    if (poziom == "gminy" && i == "146501"){
      result <- get_from_db(poziom, zmienna, "1465%", con) #sums all scores for Warsaw districts
    } else {
      result <- get_from_db(poziom, zmienna, i, con)
    }
    
    if (is.finite(result[[1]][1])){
      dodatk_wynik <- (result[[1]][1]/result[[2]][1]) * 100
    } else {
      dodatk_wynik <- NA
    }
    
    wyniki <- append(wyniki, dodatk_wynik)
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