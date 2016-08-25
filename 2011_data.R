library(openxlsx)
library(dplyr)
library(tidyr)

load_2011_csv <- function(path, file_no){
  
  # load data and fix corrupted strings
  unsorted_data <- read.csv2(paste0(path, "/p", file_no, ".csv", collapse=''),
                             skip=4, fileEncoding="Windows-1250", quote="") %>%
        apply(2, function(x) gsub(pattern='"=""', x=x, replacement='')) %>%
        apply(2, function(x) gsub(pattern='"""', x=x, replacement='')) %>%
        data.frame()
  
  # load some names scattered across 3rd row to use them as data column names
  list_names <- read.csv2(paste0(path, "/p", file_no, ".csv", collapse=''),
                          skip=3, fileEncoding="Windows-1250", quote="", nrows=1, header=FALSE) %>%
        sapply(function(x) gsub(pattern='"=""', x=x, replacement='')) %>%
        sapply(function(x) gsub(pattern='"""', x=x, replacement='')) 
  
  colnames(unsorted_data)[which(startsWith(list_names, "Lista"))] <- make.names(list_names[which(startsWith(list_names, "Lista"))])
  
  # fix corrupted column names
  colnames(unsorted_data) <- colnames(unsorted_data) %>%
        gsub(pattern='X\\.\\.\\.\\.', replacement='') %>%
        gsub(pattern='\\.\\.\\.$', replacement='') %>%
        gsub(pattern='\\.\\.\\.', replacement='\\.') %>%
        gsub(pattern='\\.\\.', replacement='\\.') 
      
  return(unsorted_data)
}

Sys.setlocale("LC_CTYPE", "pl_PL.utf8")
#openxlsx::read.xlsx(file, sheet=1)

file_no <- "11" # (in 1:41)
path <- "./raw_data/wyniki/2011/2011-kand-obw"

unsorted_data <- load_2011_csv(path, file_no)

# add useful columns and remove irrelevant/empty ones
unsorted_data$Kod.wojewodztwo <- substr(unsorted_data$Kod.terytorialny.gminy, 1, 2)
unsorted_data$Kod.powiat <- substr(unsorted_data$Kod.terytorialny.gminy, 1, 4)
unsorted_data$Nr.okr. <- file_no
unsorted_data <- unsorted_data %>%
  select(-Lp., -X)

#segregate all columns basing on their type
komisje_columns <- c(which(colnames(unsorted_data) %>% startsWith('Liczba')),
                     which(colnames(unsorted_data) %in% c("Komisja.otrzymała.kart.do.głosowania", "Nie.wykorzystano.kart.do.głosowania")))
razem_columns <- which(colnames(unsorted_data) %>% startsWith("Razem")) # to be dropped - contains same data as lista_columns
lista_columns <- which(colnames(unsorted_data) %>% startsWith("Lista"))
info_columns <- which(colnames(unsorted_data) %in% c("Kod.terytorialny.gminy", "Gmina", "Powiat", "Województwo",
                                                     "Nr.obwodu.głosowania", "Kod.wojewodztwo", "Kod.powiat", "Nr.okr.",
                                                     "Siedziba.Obwodowej.Komisji.Wyborczej"))
surname_columns <- which(!1:ncol(unsorted_data) %in% c(komisje_columns, lista_columns, info_columns, razem_columns))

#segregate data into relevant tables  
komisje <- unsorted_data %>%
  select(info_columns, komisje_columns, lista_columns)

wyniki <- unsorted_data %>%
  select(info_columns, surname_columns)

wyniki <- wyniki %>%
  gather(Kandydat, Wynik, (length(info_columns)+1):ncol(wyniki)) # %>%
  #fix_corrupted_surnames(unsorted_data, surname_columns, file)