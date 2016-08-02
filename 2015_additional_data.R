#Get additional info from PKW

#1) 'kandydaci' data table <- info from additional .xls file
#2) 'okregi' data table <- election districts info from PKW's website

require(XML)
require(XLConnect)
require(dplyr)
require(tidyr)
require(DBI)
require(RSQLite)

##################################################
#1) Load the 'kandsejm' file
##################################################
file_path <-"/home/ev/wybory/raw_data/wyniki/2015/kandsejm2015-10-19-10-00.xls"

wb <- loadWorkbook(file_path)
kand_data <- readWorksheet(wb, sheet=1, header=TRUE)
rm(wb)

kand_data <- unite(kand_data, col=Kandydat, Imiona, Nazwisko, sep=" ")

##################################################
#2) Get the table from the website
#...might be useful (?)
##################################################

okregi <- readHTMLTable(doc="http://parlament2015.pkw.gov.pl/355_Wyniki_Sejm_XLS", header = TRUE, stringsAsFactors = FALSE)
okregi <- okregi[[1]] %>%
  select(-Plik)

##################################################
#3) Export to sqlite database
##################################################
db_filepath <- "wyniki2015.sqlite3"

con <- dbConnect(SQLite(), db_filepath)

dbWriteTable(con, name="okregi", val=okregi)

dbWriteTable(con, name="kandydaci", val=kand_data)

dbDisconnect(con)