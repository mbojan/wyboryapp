#Download and set up locally the data for the shiny app

#Choose the data you need by passing proper args to set_up()

source("maps.R")
source("2015_data.R")
source("2011_data.R")

path_2011 <- "./raw_data/wyniki/2011/2011-kand-obw"
#local directory! data obtained by direct request to the polish national election comittee PKW

set_up <- function(maps=TRUE, data_2015=FALSE, data_2011=FALSE){

  #1) Election results data from parlament2015.pkw.gov.pl
  
  if (data_2015){
    dir.create("./../visual/data")
    set_up_data_2015(dbpath = "./../visual/data/wyniki2015.sqlite3")
  }
  
  #2) Administrative maps data from codgik.gov.pl
  
  if (maps){
    dir.create("./../visual/data/maps")
    set_up_maps(outputdir = "./../visual/data/maps")
  }
  
  if (data_2011){
    set_up_data_2011(path = path_2011, dbpath = "./../visual/data/wyniki2011.sqlite3")
  }
}

set_up()