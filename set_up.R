#Download and set up locally the data for the shiny app

#Choose the data you need by passing proper args to set_up()

source("maps.R")
source("2015_data.R")

set_up <- function(maps=FALSE, results=FALSE){

  #1) Election results data from parlament2015.pkw.gov.pl
  
  if (results==TRUE){
    dir.create("./visual/data")
    set_up_data_2015(dbpath = "./visual/data/wyniki2015.sqlite3")
  }
  
  #2) Administrative maps data from codgik.gov.pl
  
  if (maps==TRUE){
    dir.create("./visual/data/maps")
    set_up_maps(outputdir = "./visual/data/maps")
  }
}

set_up(maps=FALSE, results=TRUE)
