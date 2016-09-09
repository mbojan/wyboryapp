#For the 2015 dataset
#Get and clean the data from:
#   1) 41 .xlsx files with election results
#   2) one .xls file with additional candidates data
#   3) one html table with election districts boundaries info

library(plyr)
library(dplyr)
library(tidyr)
library(DBI)
library(RSQLite)
library(openxlsx) #seems to be more efficient with larger files xlsx files than XLConnect - doesn't use Java so no memory issues
library(XLConnect) #for the single .xls file that openxlsx can't read
library(XML)

#************************************************************

format_columns <- function(dataset, columns, file_no){
  
  for (j in columns){
    if (length(unique(dataset[ ,j]))==1 && unique(dataset[ ,j])[1]=="XXXXX"){
      cat("District: ", file_no, ", disqualified/revoked: ", colnames(dataset)[j], "\n", sep="")
      dataset[ ,j] <- NA
    }
    dataset[ ,j] <-  dataset[ ,j] %>% as.integer()
  }
  return(dataset)
}

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
  data_to_fix <- dplyr::rename(data_to_fix, Kandydat=uncorrupted)
  
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
  sejm_columns <- which(colnames(unsorted_data) %>% startsWith('Sejm.'))
  Razem_columns <- which(colnames(unsorted_data) %>% startsWith('Razem.'))
  KW_columns <- c(last(sejm_columns)+1, Razem_columns[1:(length(Razem_columns)-1)] + 1)  #to be dropped - contains same data as Razem_columns
  info_columns <- which(colnames(unsorted_data) %in% c("Nazwa.komisji", "Symbol.kontrolny",
                                                      "Gmina", "TERYT.gminy",
                                                      "Kod.wojewodztwo", "Kod.powiat",
                                                      "Numer.obwodu", "Nr.okr."))
  surname_columns <- which(!1:ncol(unsorted_data) %in% c(Razem_columns, sejm_columns, info_columns, KW_columns))
  
  #format numeric columns - set type to integer and change disqualified parties/candidates results from "XXXXX" to NA
  unsorted_data <- unsorted_data %>%
        format_columns(c(Razem_columns, sejm_columns, 
                         surname_columns, which(colnames(unsorted_data)=="Numer.obwodu")),
                      file_no)
  
  #segregate data into relevant tables  
  komisje <- unsorted_data %>%
        select(info_columns, sejm_columns, Razem_columns)
  
  #to standardize column names across data from various elections
  colnames(komisje) <- colnames(komisje) %>%
    gsub(pattern="Sejm.-.", replacement="") %>%
    gsub(pattern="Razem.", replacement="") %>%
    gsub(pattern="TERYT", replacement="Kod.terytorialny")

  wyniki <- unsorted_data %>%
        select(info_columns, surname_columns)
  wyniki <- wyniki %>%
        gather(Kandydat, Wynik, (length(info_columns)+1):ncol(wyniki)) %>%
        fix_corrupted_surnames(unsorted_data, surname_columns, file)
  
  #export data to SQLite database
  dbWriteTable(con, name="wyniki", val=wyniki, append=T)
  return(komisje)
}

#************************************************************

process_cand_file_2015 <- function(con, zipfile){
  
  temp_dir <- tempdir()
  
  unzip(zipfile, files="kandsejm2015-10-19-10-00.xls", exdir=temp_dir)
  
  wb <- XLConnect::loadWorkbook(paste0(temp_dir, "/kandsejm2015-10-19-10-00.xls", collapse=''))
  cand_data <- XLConnect::readWorksheet(wb, sheet=1, header=TRUE)
  
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
  
  komisje_wszystkie <- data.frame()
  
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
    
    kom <- process_file_2015(temp, con, i) #separates data from file into two tables, writes one into db, returns the other one
    
    komisje_wszystkie <- plyr::rbind.fill(komisje_wszystkie, kom)
    
    file.remove(temp)
    cat("Finished processing election results file: ", i, "/41\n", sep="")
  }
  
  dbWriteTable(con, name="komisje", val=komisje_wszystkie)
  rm(komisje_wszystkie)
  
  temp <- tempfile()
  download.file("http://parlament2015.pkw.gov.pl/kandydaci.zip",
                destfile = temp)
  process_cand_file_2015(con, temp) #exports the data into the db
  file.remove(temp)
  
  get_district_info_2015(con)
  
  cat("Finished processing all 2015 files\n")
  dbDisconnect(con)
}
