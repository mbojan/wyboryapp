library(dplyr)
library(DBI)
library(RSQLite)

source("utils.R")


process_coords_file <- function(filename){
  coords_2015 <- read.csv2(filename, encoding="Windows-1250", as.is = TRUE) %>%
          dplyr::select(-sejm, -senat)
  
  names(coords_2015) <- names(coords_2015) %>%
    sapply(function(x) simpleCap(x)) %>%
    gsub(pattern="Dat_coord_", replacement="") %>%
    gsub(pattern="_", replacement=".") %>%
    gsub(pattern="Mwg6", replacement="Kod.terytorialny.gminy")
    
  coords_2015$x <- as.numeric(coords_2015$x)
  coords_2015$y <- as.numeric(coords_2015$y)
  coords_2015$Kod.terytorialny.gminy <- as.character(coords_2015$Kod.terytorialny.gminy) %>%
    sapply(function(x) ifelse(test=(nchar(x)==6), yes=x, no=paste0("0", x, collapse=''))) %>%
    unname()
  
  con <- dbConnect(SQLite(), "./visual/data/wyniki2015.sqlite3")
  dbGetQuery(con, "SELECT [Gmina], [Kod.terytorialny.gminy], [Nazwa.komisji], [Numer.obwodu] FROM komisje;") -> komisje
  
  coords_2015 <- base::merge(coords_2015, komisje, all=FALSE)
  
  coords_2015 <- coords_2015 %>%
    dplyr::filter(!grepl(x=coords_2015$Gmina, "Statki")) %>%
    dplyr::filter(Gmina != "Zagranica")

  return(coords_2015)
}

filename <- "./raw_data/komisje_sejm_2015.csv"
coords_2015 <- process_coords_file(filename)
saveRDS(coords_2015, "./../visual/data/coords_2015.rds")

