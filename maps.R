library(rmapshaper)
library(rgdal)
library(tidyr)

source("utils.R")

#************************************************************

get_maps <- function(path){
  
  temp <- tempfile()
  download.file("ftp://91.223.135.109/prg/jednostki_administracyjne.zip", #URL works as of 10 Aug 2016
                destfile = temp)
  
  Sys.setlocale('LC_ALL','C') 
  unzip(temp, exdir=path, junkpaths = TRUE)
  file.remove(temp)
  
  previous_dir <- getwd()
  setwd(path)
  
  for (i in list.files()){
    if (grepl(pattern="obreby", i)){
      file.remove(i)
    }
    if (grepl(pattern="Pa.+stwo", i)){
      file.rename(i, gsub(pattern="Pa.+stwo", replacement = "panstwo", i))
    }
    if (grepl(pattern="wojew.+twa", i)){
      file.rename(i, gsub(pattern="wojew.+twa", replacement = "wojewodztwa", i))
    }
  }
  
  setwd(previous_dir)
  rm(previous_dir)
}

#************************************************************

load_map <- function(name, path){
  
  Sys.setlocale('LC_ALL','C') 
  map <- readOGR(paste0(path, "/", name, ".shp", collapse=""), name, use_iconv = TRUE, encoding ="Windows-1250")
  
  map@data <- map@data[, c(5, 6, 16)]
  names(map@data) <- c("code", "name", "area")
  
  if (name != "wojewodztwa" && name != "panstwo"){             #area data available only for these two
      map@data <- map@data[, -3]    
  }
  
  if (any(duplicated(map@data$code))){
    warning("Duplicated entries found in ", name, ".shp!")
    cat("Following duplicates found in ", name, ".shp:\n")
    print(find_duplicated(map@data, columns=1))
  }
  
  if (name == "gminy"){
    map@data <- map@data %>%
      separate("code", into=c("code", "type"), sep = 6)
  }
  
  if (name == "jednostki_ewidencyjne"){
    map@data <- map@data %>%
      separate("code", into=c("code", "type"), sep = "_")
  }
  
  return(map)
}

#************************************************************
set_up_maps <- function(outputdir){
  
  temppath = tempdir()
  
  get_maps(temppath)                                            #TO DO: unzip has problems with encoding - does it affect the file contents? also, will this work on windows?
  
  load_map(name="wojewodztwa", temppath) %>%
          ms_simplify(keep=0.01) %>%
          spTransform(CRS("+init=epsg:4326")) %>%               #this line converts data to usable coordinate system
          saveRDS(paste0(outputdir, "/woj.rds", collapse=""))
  
  load_map(name="powiaty", temppath) %>%                        # TO DO: test different 'keep' values and whether all polygons are still there after simplifying
          ms_simplify(keep=0.01) %>%                            # for wojewodztwa.shp Hel Peninsula disappears around keep=0.06
          spTransform(CRS("+init=epsg:4326")) %>%
          saveRDS(paste0(outputdir, "/powiaty.rds", collapse=""))
  
  load_map(name="panstwo", temppath) %>%
          ms_simplify(keep=0.01) %>%
          spTransform(CRS("+init=epsg:4326")) %>%
          saveRDS(paste0(outputdir, "/panstwo.rds", collapse=""))
  
  gminy <- load_map(name="gminy", temppath) %>%                 # TO DO: there seem to be a few duplicated entries in the gminy.shp file
          ms_simplify(keep=0.01) %>%
          spTransform(CRS("+init=epsg:4326"))
  
  saveRDS(gminy, paste0(outputdir, "/gminy.rds", collapse=""))
  
  gminy_teryt <- gminy$TERYT
  rm(gminy)
  
  miasta <- load_map(name="jednostki_ewidencyjne", temppath)
  miasta <- miasta[!(miasta$TERYT %in% gminy_teryt),]
  
  miasta %>%
          ms_simplify(keep=0.01) %>%
          spTransform(CRS("+init=epsg:4326")) %>%
        	subset(type==8) %>%                                         #dropping Krakow and Lodz maps
        	saveRDS(paste0(outputdir, "/warszawa.rds", collapse=""))
  
 unlink(temppath)
}