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
    cat("Following duplicates found in ", name, ".shp:\n", sep="")
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
  
  get_maps(temppath)
  
  for (i in c("panstwo", "wojewodztwa", "powiaty", "gminy", "jednostki_ewidencyjne")){
    
    temp <- load_map(name = i, temppath)
    name <- i
    
    if (i == "jednostki_ewidencyjne"){                                # extracts Warsaw district data from "jednostki_ewidencyjne.shp"
      temp <- subset(temp, type == 8)                                 # Krakow and Lodz maps can also be found here ('type == 9')
      name <- "warszawa"
    }
    
    temp %>%
      rmapshaper::ms_simplify(keep = 0.01) %>%                        # simplifies the polygons to reduce map size
      sp::spTransform(CRS("+init=epsg:4326")) %>%                     # converts map data to usable coordinate system
      saveRDS(get(i), paste0(outputdir, "/", name, ".rds", collapse=""))
  }
  
  unlink(temppath)
}