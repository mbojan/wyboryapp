#For the 2015 dataset
#Get and clean the data from:
#   1) 41 .xlsx files with election results
#   2) one .xls file with additional candidates data
#   3) one html table with election districts boundaries info

library(dplyr)
library(tidyr)
library(DBI)
library(RSQLite)
library(openxlsx) #seems to be more efficient with larger files xlsx files than XLConnect - doesn't use Java so no memory issues
library(XLConnect) #for the single .xls file that openxlsx can't read
library(XML)

#************************************************************

fix_corrupted_surnames <- function(data_to_fix, data_for_reference, surnames_cols, file){

  corrupted <- data.frame(colnames(data_for_reference)[surnames_cols])
  colnames(corrupted)[1] <- "corrupted"
  
  uncorrupted <- openxlsx::read.xlsx(file, sheet=1, colNames=FALSE, rows=1:2)
  
  uncorrupted <- t(uncorrupted[1, surnames_cols])
  rownames(uncorrupted) <- NULL
  colnames(uncorrupted)[1] <- "uncorrupted"
  
  surnames <- cbind(corrupted, uncorrupted)
  
  #merge tables with corrupted and uncorrupted names and remove the corrupted version
  data_to_fix <- merge(data_to_fix, surnames, all.x=TRUE, by.x="Kandydat", by.y="corrupted")
  data_to_fix <- select(data_to_fix, -Kandydat)
  data_to_fix <- rename(data_to_fix, Kandydat=uncorrupted)
  
  return(data_to_fix)
}

#************************************************************

process_file_2015 <- function(file, con, file_no){
  
  unsorted_data <- openxlsx::read.xlsx(file, sheet=1)
  
  #add useful columns
  unsorted_data$Kod.wojewodztwo <- substr(unsorted_data$TERYT.gminy, 1, 2)
  unsorted_data$Kod.powiat <- substr(unsorted_data$TERYT.gminy, 1, 4)
  unsorted_data$Nr.okr. <- file_no
  
  #segregate all columns basing on their type
  sejm_columns <- (1:ncol(unsorted_data))[colnames(unsorted_data) %>% startsWith('Sejm.')]
  Razem_columns <- (1:ncol(unsorted_data))[colnames(unsorted_data) %>% startsWith('Razem.')]
  KW_columns <- c(last(sejm_columns)+1, Razem_columns[1:(length(Razem_columns)-1)] + 1)
  info_columns <- (1:ncol(unsorted_data))[colnames(unsorted_data) %in% c("Nazwa.komisji",
                                                                        "Symbol.kontrolny",
                                                                        "Gmina",
                                                                        "TERYT.gminy",
                                                                        "Kod.wojewodztwo",
                                                                        "Kod.powiat",
                                                                        "Numer.obwodu",
                                                                        "Nr.okr.")]
  surname_columns <- (1:ncol(unsorted_data))[!1:ncol(unsorted_data) %in% c(Razem_columns, sejm_columns, info_columns, KW_columns)]
  
  #change type to integer wherever it's needed
  for (j in c(sejm_columns, Razem_columns)){
    if (length(unique(unsorted_data[ ,j]))==1 && unique(unsorted_data[ ,j])[1]=="XXXXX"){
      cat("District: ", file_no, ", disqualified party found: ", colnames(unsorted_data)[j], "\n")
      unsorted_data[ ,j] <- NA
    }
    unsorted_data[ ,j] <-  unsorted_data[ ,j] %>% as.integer()
  }
  unsorted_data$Numer.obwodu <- as.integer(unsorted_data$Numer.obwodu)

  #segregate data into relevant tables  
  komisje <- unsorted_data %>%
        select(info_columns, sejm_columns)
  
  komisje_KW <- unsorted_data %>%
        select(Symbol.kontrolny, Razem_columns)

  wyniki <- unsorted_data %>%
    select(info_columns, surname_columns)
  
  wyniki <- wyniki %>%
    gather(Kandydat, Wynik, (length(info_columns)+1):ncol(wyniki)) %>%
    fix_corrupted_surnames(unsorted_data, surname_columns, file)
  
  #replace "XXXXX" values (null score values of candidates who ended up not participating) with NAs
  if ("XXXXX" %in% wyniki$Wynik) {
    disqualified <- wyniki %>%
      select(Wynik, Kandydat) %>%
      filter(Wynik=="XXXXX") %>%
      unique() %>%
      select(Kandydat)
    
    wyniki$Wynik[wyniki$Kandydat %in% disqualified[[1]]] <- NA
    
    for (j in disqualified[[1]]){
      cat("District: ", file_no, ", disqualified candidate found: ", j, "\n")
    }
  }
  wyniki$Wynik <- as.integer(wyniki$Wynik)
  
  #export data to SQLite database
  dbWriteTable(con, name="wyniki", val=wyniki, append=T)
  dbWriteTable(con, name="komisje", val=komisje, append=T)
  dbWriteTable(con, name=paste0("komisje_KW", file_no, collapse=''), val=komisje_KW) #to be merged with 'komisje' table after loading all files
}

#************************************************************

process_cand_file_2015 <- function(con, zipfile){
  
  temp_dir <- tempdir()
  
  unzip(zipfile, files="kandsejm2015-10-19-10-00.xls", exdir=temp_dir)
  
  wb <- loadWorkbook(paste0(temp_dir, "/kandsejm2015-10-19-10-00.xls", collapse=''))
  cand_data <- readWorksheet(wb, sheet=1, header=TRUE)
  
  cand_data <- unite(cand_data, col=Kandydat, Imiona, Nazwisko, sep=" ")
  
  cand_data$Nr.okr. <- as.integer(cand_data$Nr.okr.)
  cand_data$Nr.listy <- as.integer(cand_data$Nr.listy)
  cand_data$Poz. <- as.integer(cand_data$Poz.)
  
  dbWriteTable(con, name="kandydaci", val=cand_data)
  
  file.remove(paste0(temp_dir, "/kandsejm2015-10-19-10-00.xls", collapse=''))
  unlink(temp_dir)
}

#************************************************************

get_district_info_2015 <- function(con){
  
  okregi <- readHTMLTable(doc="http://parlament2015.pkw.gov.pl/355_Wyniki_Sejm_XLS", header = TRUE, stringsAsFactors = FALSE)
  
  okregi <- okregi[[1]] %>%
            select(-Plik, Nr.okr. = 1)
  
  okregi[, 1] <- as.integer(okregi[, 1])
  
  dbWriteTable(con, name="okregi", val=okregi)
  
}

#************************************************************

set_up_data_2015 <- function(dbpath){

  con <- dbConnect(SQLite(), dbpath)
  
  for (i in 1:41) {
    
    temp <- tempfile()
    
    #for files 1 to 9 there is an additional '0' in the URL (01, 02 etc)
    if (i<10) {
      download.file(paste0("http://parlament2015.pkw.gov.pl/wyniki_okr_sejm/0", i, ".xlsx", collapse=""),
                    destfile = temp)
    } else {
      download.file(paste0("http://parlament2015.pkw.gov.pl/wyniki_okr_sejm/", i, ".xlsx", collapse=""),
                    destfile = temp)
    }
    
    process_file_2015(temp, con, i) #separates data from file into two tables and writes them into the db
    file.remove(temp)
    
    cat("Finished processing election results file: ", i, "/41\n")
  }
  
  temp <- tempfile()
  download.file("http://parlament2015.pkw.gov.pl/kandydaci.zip",
                destfile = temp)
  
  process_cand_file_2015(con, temp) #exports the data into the db
  file.remove(temp)
  
  get_district_info_2015(con)
  
  cat("Finished processing all 2015 files\n")
  
  dbDisconnect(con)
}

