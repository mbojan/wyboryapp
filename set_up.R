#Download and set up locally the data for the shiny app

#Choose the data you need by passing proper args to set_up()

source("maps.R")
source("2015_data.R")
source("2011_data.R")

set_up <- function(maps=FALSE, data_2015=FALSE, data_2011=FALSE){

  #1) Election results data from parlament2015.pkw.gov.pl
  
  if (data_2015==TRUE){
    dir.create("./visual/data")
    set_up_data_2015(dbpath = "./visual/data/wyniki2015.sqlite3")
  }
  
  #2) Administrative maps data from codgik.gov.pl
  
  if (maps==TRUE){
    dir.create("./visual/data/maps")
    set_up_maps(outputdir = "./visual/data/maps")
  }
  
  if (data_2011 == TRUE){
    set_up_data_2011(path = "./raw_data/wyniki/2011/2011-kand-obw", dbpath = "./visual/data/wyniki2011.sqlite3")
  }
}

set_up(maps=FALSE, data_2015=TRUE, data_2011 = FALSE)

# by default, this won't process maps -> prepared maps uploaded to github, no need to set them up from scratch
