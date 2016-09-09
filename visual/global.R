library(sp)
library(leaflet)
library(DBI)
library(RSQLite)
library(htmltools)

#************************************************************

get_from_db <- function(given_level, given_var, con, given_year){

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
    
  FROM_table <- paste0(" FROM komisje", given_year, collapse = '')
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

find_results <- function(given_var, given_level, code, con, given_year){
  
  result_data <- get_from_db(given_level, given_var, con, given_year)
  
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
