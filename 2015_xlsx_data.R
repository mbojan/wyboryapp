#For the 2015 dataset
#Get and clean the data from the 41 .xlsx files

#Separating data into:
#'komisje' data table <- all local commissions info columns and collective party scores ('Razem...' columns)
#'wyniki' data table <- basic commissions info and every observation (scores of every single candidate for every single commission)

require(dplyr)
require(tidyr)
require(DBI)
require(RSQLite)
require(openxlsx) #seems to be more efficient with larger files than XLConnect - doesn't use Java so no memory issues

path <- "/home/ev/wybory/raw_data/wyniki/2015/"
number_of_datasets <- 41                                #number of election districts
database_name <- "wyniki2015.sqlite3"                   #name for created .sqlite3 file

#connect to SQLite
con <- dbConnect(SQLite(), database_name)

###################################################################
#repeat long procedure for all 41 files

for (i in 1:number_of_datasets) {
  
  #1) find file path of the current file
  
  #get double digit number e.g. 01 instead of 1
  if (i<10) {
    i_string<-paste0("0", i, collapse='')
  } else {
    i_string<-paste0(i)
  }
  
  current_file <- paste0(path, i_string, ".xlsx", collapse='')
  rm(i_string)
  
  #2) get data from file

  temp<-openxlsx::read.xlsx(current_file, sheet=1)
  
  #3) sort the data
  
  #find the indices of columns that don't contain candidates surnames
  #that is: first 28 columns + the last column + some irregular ones
  
  non_cand <- c(1:28, ncol(temp))

  for (j in 1:(ncol(temp)-1)) {
    if (names(temp[j]) %>% startsWith('Razem.')){
      non_cand <- c(non_cand, j, j+1)
    }
  }
  
  #separate data adjustingly

  komisje <- temp %>%
      select( Nazwa.komisji,
              Symbol.kontrolny,
              Gmina,
              TERYT.gminy,
              Numer.obwodu,
              starts_with('Sejm.'),
              starts_with('Razem.')
      )
  
  wyniki <- cbind(
    select(temp, 1:5),
    select(temp, -non_cand)
  )
  
  # gather into long data form
  wyniki <- wyniki %>% gather(Kandydat, Wynik, 6:ncol(wyniki))
  komisje <- komisje %>% gather(Zmienna, Wartosc,
    -Nazwa.komisji,
    -Symbol.kontrolny,
    -Gmina,
    -TERYT.gminy,
    -Numer.obwodu)
  
  #4) fix corrupted surnames
  
  #get our corrupted names and match them with uncorrupted versions (restore them from .xlsx column names)
  corrupted <- data.frame(colnames(temp)[-non_cand])
  colnames(corrupted)[1] <- "corrupted"
  
  rm(temp)  #won't be needed anymore
  
  uncorrupted <- openxlsx::read.xlsx(current_file, sheet=1, colNames=FALSE, rows=1:2)
  
  uncorrupted <- t(uncorrupted[1,-non_cand])
  rownames(uncorrupted) <- NULL
  colnames(uncorrupted)[1] <- "uncorrupted"
  
  surnames <- cbind(corrupted, uncorrupted)
  rm(corrupted)
  rm(uncorrupted)
  
  #merge (left join) tables with corrupted and uncorrupted names and remove the corrupted version
  wyniki <- merge(wyniki, surnames, all.x=TRUE, by.x="Kandydat", by.y="corrupted")
  wyniki <- select(wyniki, -Kandydat)
  wyniki <- rename(wyniki, Kandydat=uncorrupted)
  
  rm(surnames)
  
  #5) add 'district' data column
  wyniki$Nr.okr. <- i
  komisje$Nr.okr. <- i
    
  #6) export data to SQLite database
  dbWriteTable(con, name="wyniki", val=wyniki, append=T)
  dbWriteTable(con, name="komisje", val=komisje, append=T)

  #7) clean up
  rm(wyniki)
  rm(komisje)
  gc()
  
  cat("Finished processing file: ", i, "/", number_of_datasets, "\n")
}
###################################################################

dbDisconnect(con)