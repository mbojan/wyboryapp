library(sp)
library(leaflet)
library(DBI)
library(RSQLite)

#************************************************************

get_from_db <- function(given_level, given_var, con){
  
  level_code <- switch(given_level,
                   "warszawa" = "[TERYT.gminy]",
                   "gminy" = "[TERYT.gminy]",
                   "powiaty" = "[Kod.powiat]",
                   "wojewodztwa" = "[Kod.wojewodztwo]",
                   "panstwo" = NA)
  
  SELECT_sums <- paste0("SELECT SUM([", given_var, "]),
                  SUM([Sejm.-.Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkie.listy.kandydatów]) ",
                  collapse='')
  query <- SELECT_sums
  
  if (!is.na(level_code)){
    SELECT_code <- paste0(", ", level_code, " ", collapse = '')
    query <- paste0(query, SELECT_code, collapse = '')
  }
    
  FROM_table <- paste0(" FROM komisje", collapse = '')
  query <- paste0(query, FROM_table, collapse='')
  
  if (given_level == "warszawa"){
    is_warsaw <- " WHERE [TERYT.gminy] LIKE '1465%'"
    query <- paste0(query, is_warsaw, collapse='')
  }
  
  if (!is.na(level_code)){
    GROUP_BY <- paste0(" GROUP BY ", level_code, collapse = '')
    query <- paste0(query, GROUP_BY, collapse = '')
  }
  
  result <- data.frame(dbGetQuery(con, query))
  return(result)
}

#************************************************************

find_results <- function(given_var, given_level, code, con){
  
  result_data <- get_from_db(given_level, given_var, con)
  result_data$percent_scores <- (as.integer(result_data[, 1])*100)/result_data[, 2]
  
  if(given_level != "panstwo"){
    
    #discard area codes that can't be found in the Polish map data (from ballots located on ships and abroad)
    result_data <- result_data[result_data[, 3] %in% as.character(code),]
    
    #results for all individual regions need to be ordered in the same way as the regions appear in the map data
    result_data <- base::merge(data.frame(code), result_data, all.x=TRUE, by.x="code", by.y=names(result_data)[3], sort=FALSE)
  }
  return(result_data$percent_scores)
}
  
#************************************************************
  
draw_map <- function(map, percent_scores, given_level){
  
  shades <- colorRampPalette(c("white", "darkblue"))(101)
  map_colors <- vector()

  index_na <- which(is.na(percent_scores))
  index_else <- which(!is.na(percent_scores))
  
  map_colors[index_na]  <- "gray30"
  map_colors[index_else] <- shades[round(percent_scores[index_else])+1] #shades vector is in 1:101 range, results are in 0:100
  
  if (given_level== "warszawa"){
    view <- c(21.05, 52.24, 10)
  } else {
    view <- c(19.27, 52.03, 6)
  }
  
  leaflet() %>%
    addTiles() %>%
    addPolygons(data=map, stroke = TRUE, weight=0.5, color="black", fillOpacity = 0.5, smoothFactor = 0.5, fillColor=map_colors) %>%
    setView(lng = view[1], lat = view[2], zoom = view[3])
  
}