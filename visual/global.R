library(sp)
library(leaflet)
library(DBI)
library(RSQLite)
library(htmltools)

#************************************************************

get_from_db <- function(given_level, given_var, con){

  level_code <- switch(given_level,
                   "warszawa" = "[Kod.terytorialny.gminy]",
                   "gminy" = "[Kod.terytorialny.gminy]",
                   "powiaty" = "[Kod.powiat]",
                   "wojewodztwa" = "[Kod.wojewodztwo]",
                   "panstwo" = NA)
  
  if (given_var == "Frekwencja"){
    var1 <- "Liczba.kart.ważnych"
    var2 <- "Liczba.wyborców.uprawnionych.do.głosowania"
  } else {
    var1 <- given_var  
    var2 <- "Liczba.głosów.ważnych.oddanych.łącznie.na.wszystkie.listy.kandydatów"
  }
  
  SELECT_sums <- paste0("SELECT SUM([", var1, "]),
                   SUM([", var2, "]) ",
                  collapse='')
  
  query <- SELECT_sums
  
  if (!is.na(level_code)){
    SELECT_code <- paste0(", ", level_code, " ", collapse = '')
    query <- paste0(query, SELECT_code, collapse = '')
  }
    
  FROM_table <- paste0(" FROM komisje", collapse = '')
  query <- paste0(query, FROM_table, collapse='')
  
  if (given_level == "warszawa"){
    is_warsaw <- " WHERE [Kod.terytorialny.gminy] LIKE '1465%'"
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
  
  if (given_level == "gminy"){
    #aggregate scores of all Warsaw districts
    warsaw <- which(startsWith(result_data$Kod.terytorialny.gminy, "1465"))
    
    result_data <- rbind(result_data[-warsaw,], 
                         c(sum(as.integer(result_data[warsaw,1])), sum(as.integer(result_data[warsaw,2])), "146501"))
  }
  
  result_data$scores <- (as.integer(result_data[, 1])*100)/as.integer(result_data[, 2])
  
  if(given_level != "panstwo"){
    
    #discard area codes that can't be found in the Polish map data (from ballots located on ships and abroad)
    result_data <- result_data[result_data[, 3] %in% as.character(code),]
    
    #results for all individual regions need to be ordered in the same way as the regions appear in the map data
    result_data <- base::merge(data.frame(code), result_data, all.x=TRUE, by.x="code", by.y=names(result_data)[3], sort=FALSE)
  }
  return(result_data$scores)
}
  
#************************************************************
  
draw_map <- function(map, scores, given_level, min, max, color, coords){
  
  index_na <- which(is.na(scores))
  index_else <- which(!is.na(scores))
  
  scaled_scores <- scores
  
  scaled_scores[index_else] <- pmax(scaled_scores[index_else], min)
  scaled_scores[index_else] <- pmin(scaled_scores[index_else], max)
  
  shades <- colorRampPalette(c("white", color))(max-min+1)
  map_colors <- vector()
  
  map_colors[index_na]  <- "gray30"
  map_colors[index_else] <- shades[round(scaled_scores[index_else])-min+1]
  
  if (given_level == "warszawa"){
    view <- c(21.05, 52.24, 11)
  } else {
    view <- c(19.27, 52.03, 7)
  }

  scores_text <- round(scores, 2) %>%
    sapply(function(x) paste0(x, "%", collapse='')) %>%
    gsub(pattern="NA%", replacement="brak wyniku")
  
  leaflet() %>%
    addTiles(group="OpenStreetMap") %>%
    addProviderTiles("CartoDB.Positron", group="CartoDB") %>%
    addPolygons(data=map, stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, fillColor=map_colors) %>%
    addPolygons(data=map, stroke = TRUE, weight=0.5, color="black", group="Wyświetl granice", fillOpacity = 0) %>%
    addPolygons(data=map, stroke=FALSE, fillOpacity=0,
                    label = unname(mapply(function(x, y) {HTML(sprintf("%s<br>%s", htmlEscape(x), htmlEscape(y)))},
                    map$name, scores_text, SIMPLIFY = F))) %>%
    setView(lng = view[1], lat = view[2], zoom = view[3]) %>%
    addLayersControl(baseGroups = c("OpenStreetMap", "CartoDB"), overlayGroups=c("Wyświetl granice", "Wyświetl komisje"),
                     options=layersControlOptions(collapsed=FALSE), position = "bottomleft") #%>%
    #addMarkers(label=unname(mapply(function(x, y, z) 
    #                {HTML(sprintf("%s<br>Numer obwodu: %i<br>Nazwa komisji: %s", htmlEscape(x), htmlEscape(y), htmlEscape(z)))},
    #                coords$Gmina, coords$Numer.obwodu, coords$Nazwa.komisji, SIMPLIFY = F)),
    #                lng=coords$x, lat=coords$y, clusterOptions = markerClusterOptions(), group="Wyświetl komisje")
}
