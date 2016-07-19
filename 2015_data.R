#For the 2015 dataset
#Separating data into:
#'komisje' data table <- all local commissions info columns and collective party scores ('Razem...' columns)
#'wyniki' data table <- basic commissions info and every observation (scores of every single candidate for every single commission)

require(dplyr)
require(tidyr)
require(XLConnect)
require(DBI)
require(RSQLite)

#variables
year <- 2015
path <- "/home/ev/wybory/raw_data/wyniki/"
liczbaOkregow <- 41

#connect to SQLite
con <- dbConnect(SQLite(), paste0("wyniki", year, ".sqlite3", collapse=''))

#looping through all the .xls files
for (i in 1:liczbaOkregow) {
  
  #get double digit number
  if (i<10)
    i<-paste0("0", i, collapse='')
  
  #get data from file into 'temp' variable
  wb <- loadWorkbook(paste0(path, year, "/", i, ".xlsx", collapse=''))
  temp <- readWorksheet(wb, sheet=1, header=T)
  rm(wb)
 
  #find the indices of some unnecessary irregular columns
  indeksy <- vector()
  #we need to find the one column after all "Sejm..." columns
  #and then every next column after 'Razem...' column (except the last one)
    
  for (j in 1:(ncol(temp)-1)) {
    if (names(temp[j]) %>% startsWith('Sejm.')){
      indeksy <- j+1
    }
    if (names(temp[j]) %>% startsWith('Razem.')){
      indeksy <- c(indeksy, j+1)
    }
  }
   
  #sort the data
  komisje <- temp %>%
      select( Nazwa.komisji,
              Symbol.kontrolny,
              Gmina,
              TERYT.gminy,
              Numer.obwodu,
              starts_with('Sejm.'),
              starts_with('Razem.')
      )
  
  wyniki <- temp %>%
    #watch out for the order! this works only if 'indeksy' comes first
      select( -indeksy,
              -starts_with('Sejm.'),
              -starts_with('Razem.')
      )
  
  rm(temp)
 
  #gather into long data form
  wyniki <- wyniki %>% gather(Kandydat, Wynik, 6:ncol(wyniki))
  
  #split names into 'name' and 'surname' columns (assuming candidates have at most 3 first names - a warning will pop up otherwise)
  wyniki <- wyniki %>% separate(into=c("a", "b", "c", "Nazwisko"), sep="\\.", col=Kandydat, remove = TRUE, fill = "left")
  wyniki <- wyniki %>% unite (col="Imiona", a, b, c, sep=" ")
  wyniki$Imiona <- gsub("NA ", "", wyniki$Imiona)

  #export data to SQLite database
  dbWriteTable(con, name=paste0("wyniki", i, collapse=''), val=wyniki)
  dbWriteTable(con, name=paste0("komisje", i, collapse=''), val=komisje)

  #clean up
  rm(wyniki)
  rm(komisje)
  gc()
  
  cat("Finished processing file: ", i, "/", liczbaOkregow, "\n")
}