#For the 2015 dataset
#Separating data into:
#'komisje' data table <- all local commissions info columns and collective party scores ('Razem...' columns)
#'wyniki' data table <- basic commissions info and every observation (scores of every single candidate for every single commission)
#'kandydaci' data table <- info from additional .xls file
#'okregi' data table <- election districts info from PKW's website

require(dplyr)
require(tidyr)
require(DBI)
require(RSQLite)
require(XML)

source("utils.R")


#variables
path <- "/home/ev/wybory/raw_data/wyniki/2015/"
number_of_datasets <- 41                                #number of election districts
kandydaci_filename <-"kandsejm2015-10-19-10-00.xls"
database_name <- "wyniki2015"                           #name for created .sqlite3 file


#connect to SQLite
con <- dbConnect(SQLite(), paste0(database_name, ".sqlite3", collapse=''))

#*************************************************************************

#get election district data
okregi <- readHTMLTable(doc="http://parlament2015.pkw.gov.pl/355_Wyniki_Sejm_XLS", header = TRUE)
okregi <- okregi[[1]] %>%
  select(-Plik)

#export data to SQLite database
dbWriteTable(con, name="okregi", val=okregi)
rm(okregi)

cat("Finished processing district data.\n\n")

#*************************************************************************

#get candidates data
kand_data <- get_xls_data(paste0(path, kandydaci_filename, collapse=''))

#export data to SQLite database
dbWriteTable(con, name="kandydaci", val=kand_data)
rm(kand_data)

cat("Finished processing candidates data.\n\n")

#*******************************************************************

#process the rest of the files (results from all election districts)

for (i in 1:number_of_datasets) {
  
  #get double digit number e.g. 01 instead of 1
  if (i<10) {
    i_string<-paste0("0", i, collapse='')
  } else {
    i_string<-paste0(i)
  }
  
  #get data from file
  temp <- get_xls_data(paste0(path, i_string, ".xlsx", collapse=''))
 
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
  
  #split names into 'name' and 'surname' columns
  #TO DO

  #add 'district' data column
  wyniki$Nr.okr. <- i
  komisje$Nr.okr. <- i
    
  #export data to SQLite database
  dbWriteTable(con, name="wyniki", val=wyniki, append=T)
  dbWriteTable(con, name=paste0("komisje", i_string, collapse=''), val=komisje)

  #clean up
  rm(wyniki)
  rm(komisje)
  gc()
  
  cat("Finished processing file: ", i, "/", number_of_datasets, "\n")
}

dbDisconnect(con)