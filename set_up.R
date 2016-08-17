#Set up locally everything *from scratch*

#Download and process all the necessary data for the shiny app

source("maps.R")
source("2015_data.R")

set_up <- function(){

  #1) Election results data from parlament2015.pkw.gov.pl
  dir.create("./visual/data")
  set_up_data_2015(dbpath = "./visual/data/wyniki2015.sqlite3")
  
  #2) Administrative maps data from codgik.gov.pl
  dir.create("./visual/data/maps")
  set_up_maps(outputdir = "./visual/data/maps")
}

set_up()

