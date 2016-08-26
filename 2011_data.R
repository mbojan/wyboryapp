library(openxlsx)
library(plyr)
library(dplyr)
library(tidyr)
library(DBI)
library(RSQLite)

load_2011 <- function(filename){
  
  #load data and some names scattered across 3rd row to use them as data column names
  if (endsWith(filename, "csv")){
    
    #fixes corrupted strings in loaded .csv files
    unsorted_data <- read.csv2(filename, skip=4, fileEncoding="Windows-1250", quote="") %>%
      apply(2, function(x) gsub(pattern='"=""', x=x, replacement='')) %>%
      apply(2, function(x) gsub(pattern='"""', x=x, replacement='')) %>%
      data.frame()
    
    unsorted_data <- unsorted_data %>%
      select(-X)
    
    names <- read.csv2(filename, skip=3, fileEncoding="Windows-1250", quote="", nrows=1, header=FALSE) %>%
      sapply(function(x) gsub(pattern='"=""', x=x, replacement='')) %>%
      sapply(function(x) gsub(pattern='"""', x=x, replacement='')) 
  
  } else {
    unsorted_data <- openxlsx::read.xlsx(filename, sheet=1, startRow=5)
    
    # read.csv handles automatically duplicated column names, openxlsx does not
    # appending some numbers to duplicated names so that there are no issues later
    for (j in which(startsWith(colnames(unsorted_data), "Razem"))){
      colnames(unsorted_data)[j] <- paste0("Razem", j, collapse='')
    }
    
    names <- openxlsx::read.xlsx(filename, sheet=1, rows=c(4:5), colNames=FALSE)
    names <- names[1,] %>%
      as.character()
  }
  
  colnames(unsorted_data)[which(startsWith(names, "Lista"))] <- make.names(names[which(startsWith(names, "Lista"))])
  
  #fix corrupted column names
  colnames(unsorted_data) <- colnames(unsorted_data) %>%
      gsub(pattern='X\\.\\.\\.\\.', replacement='') %>%
      gsub(pattern='\\.\\.\\.$', replacement='') %>%
      gsub(pattern='\\.\\.\\.', replacement='\\.') %>%
      gsub(pattern='\\.\\.', replacement='\\.') %>%
      gsub(pattern=',\\.', replacement='\\.') 
      
  return(unsorted_data)
}

standarize_colnames <- function(dataset){
  
  colnames(dataset) <- colnames(dataset) %>%
    gsub(pattern="Lista\\.nr\\..{1,2}\\.", replacement="") %>%
    gsub(pattern="SLD", replacement="Sojusz.Lewicy.Demokratycznej") %>%
    gsub(pattern="Komitet.Wyborczy", replacement="KW") %>%
    gsub(pattern="PSL", replacement="Polskie.Stronnictwo.Ludowe") %>%
    gsub(pattern="PPP", replacement="Polska.Partia.Pracy")
  
  return(dataset)
}

process_file_2011 <- function(filename, file_no){
  
  Sys.setlocale("LC_CTYPE", "pl_PL.utf8")
  
  unsorted_data <- load_2011(filename)
  
  # add useful columns and remove irrelevant/empty ones
  unsorted_data$Kod.wojewodztwo <- substr(unsorted_data$Kod.terytorialny.gminy, 1, 2)
  unsorted_data$Kod.powiat <- substr(unsorted_data$Kod.terytorialny.gminy, 1, 4)
  unsorted_data$Nr.okr. <- file_no
  unsorted_data <- unsorted_data %>%
    select(-Lp.)
  
  #segregate all columns basing on their type
  komisje_columns <- c(which(colnames(unsorted_data) %>% startsWith('Liczba')),
                       which(colnames(unsorted_data) %in% c("Komisja.otrzymała.kart.do.głosowania", "Nie.wykorzystano.kart.do.głosowania")))
  razem_columns <- which(colnames(unsorted_data) %>% startsWith("Razem")) # to be dropped - contains same data as lista_columns
  lista_columns <- which(colnames(unsorted_data) %>% startsWith("Lista"))
  info_columns <- which(colnames(unsorted_data) %in% c("Kod.terytorialny.gminy", "Gmina", "Powiat", "Województwo",
                                                       "Nr.obwodu.głosowania", "Kod.wojewodztwo", "Kod.powiat", "Nr.okr.",
                                                       "Siedziba.Obwodowej.Komisji.Wyborczej"))
  surname_columns <- which(!1:ncol(unsorted_data) %in% c(komisje_columns, lista_columns, info_columns, razem_columns))
  
  #fix selected columns data type
  cols_to_int <- c(lista_columns, komisje_columns, which(colnames(unsorted_data)=="Nr.obwodu.głosowania"))
  
  unsorted_data[,cols_to_int] <- unsorted_data[,cols_to_int] %>%
    apply(2, function(x) as.integer(x))
  
  #segregate data into relevant tables  
  komisje <- unsorted_data %>%
    select(info_columns, komisje_columns, lista_columns)
  
  #wyniki <- unsorted_data %>%
  #  select(info_columns, surname_columns)
  
  #attributes(wyniki)
  #attributes(wyniki)[[2]] <- NULL
  #wyniki <- wyniki %>%
  #  gather(Kandydat, Wynik, (length(info_columns)+1):ncol(wyniki)) # %>%
    #fix_corrupted_surnames(unsorted_data, surname_columns, file)
  
  return(komisje)
}

set_up_data_2011 <- function(path, dbpath){
  
  con <- dbConnect(SQLite(), dbpath)
  
  komisje_wszystkie <- data.frame()
  
  for (i in 1:41) {
    temp <- tempfile()
    
    #for files 1 to 9 there is an additional '0' in the URL (01, 02 etc)
    if (i<10) {
      filename <- paste0(path, "/p0", i, ".csv", collapse="")
    } else {
      filename <- paste0(path, "/p", i, ".csv", collapse="")
    }
    
    if (i == 19){
      filename <- sub(x = filename, pattern = "csv", replacement = "xlsx")
    }
    
    kom <- process_file_2011(filename, i)
    
    kom <- standarize_colnames(kom)
    
    komisje_wszystkie <- plyr::rbind.fill(komisje_wszystkie, kom)
    cat("Finished processing election results file: ", i, "/41\n", sep="")
  }
  
  dbWriteTable(con, name="komisje", val=komisje_wszystkie)
  rm(komisje_wszystkie)
  
  dbDisconnect(con)
  }