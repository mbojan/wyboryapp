library(sp)
library(leaflet)
library(DBI)
library(RSQLite)

#************************************************************

get_from_db <- function(given_level, given_var, con){
  
  level_code <- switch(given_level,
                   "powiaty" = "Kod.powiat",
                   "wojewodztwa" = "Kod.wojewodztwo",
                   "gminy" = "TERYT.gminy",
                   "panstwo" = NA)
  
  query <- paste0("SELECT SUM([", given_var, "]),
                  SUM([Sejm.-.Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkie.listy.kandydatów])",
                  collapse='')
  
  if (!is.na(level_code)){
    query <- paste0(query, ", [", level_code, "]", collapse = '')
  }
  
  query <- paste0(query, " FROM komisje", collapse = '')
  
  if (!is.na(level_code)){
    query <- paste0(query, " GROUP BY [", level_code, "]", collapse = '')
  }
  
  result <- data.frame(dbGetQuery(con, query))
  return(result)
}

#************************************************************

find_results <- function(given_var, given_level, code, con){
  
  if (given_level == "warszawa"){
    given_level <- "gminy"
  }
  
  result_data <- get_from_db(given_level, given_var, con)
  result_data$percent_scores <- (as.integer(result_data[, 1])*100)/result_data[, 2]
  #result_data <- result_data[result_data[, 3] %in% as.character(code),] #discard results without map data (from ballots located on ships and abroad)
  
  if(given_level!="panstwo"){
    sorted_codes <- data.frame(code, 1:length(code))
    names(sorted_codes) <- c("Code.col", "Order.col")
    sorted_codes$Code.col <- as.character(sorted_codes$Code.col)
    
    sorted_codes <- sorted_codes[order(sorted_codes$Code.col),]
    result_data <- result_data[order(sorted_codes$Order.col),]
    
    if (!isTRUE(all.equal(as.character(code), result_data[,3]))){
      warning("Database codes don't match with map codes! Expect wrong map results.")
    }
  }
  return(result_data$percent_scores)
}
  
#************************************************************
  
draw_map <- function(map, percent_scores){
  
  shades <- colorRampPalette(c("white", "darkblue"))(100)
  
  map_colors <- vector()
  
  index_na <- which(is.na(percent_scores))
  index_zero <- which(percent_scores < 1)
  index_non_zero <- which(percent_scores != 0)
  
  map_colors[index_na]  <- "gray30"
  map_colors[index_non_zero] <- shades[round(percent_scores[index_non_zero])]
  map_colors[index_zero] <- shades[1]
  
  leaflet() %>%
    addTiles() %>%
    addPolygons(data=map, stroke = TRUE, weight=1, color="black", fillOpacity = 0.5, smoothFactor = 0.5, fillColor=map_colors) %>%
    setView(lng = 19.27, lat = 52.03, zoom = 6)
  
}