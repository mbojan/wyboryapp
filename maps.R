library(rmapshaper)
library(rgdal)
library(rgeos)
library(tidyr)

#************************************************************

get_maps <- function(path){
  
  temp <- tempfile()
  download.file("ftp://91.223.135.109/prg/jednostki_administracyjne.zip", #URL works as of 10 Aug 2016
                destfile = temp)
  
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
  
  map <- readOGR(path, name, stringsAsFactors = FALSE, use_iconv = TRUE, encoding ="Windows-1250")
  
  map@data <- map@data[, c(5, 6, 16)]
  names(map@data) <- c("code", "name", "area")
  
  if (name != "wojewodztwa" && name != "panstwo"){             #area data available only for these two
    map@data <- map@data[, -3]
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

remove_tiny_polygons <- function(map){
  
  tiny_polygons <- which(map$area/100<1)
  
  #quick checks if no needed data is dropped
  tiny_polygons <- tiny_polygons[map$name[tiny_polygons] %in% map$name[-tiny_polygons]]
  tiny_polygons <- tiny_polygons[map$code[tiny_polygons] %in% map$code[-tiny_polygons]] 
  
  warning("Auto removing ", length(tiny_polygons), " entries with area smaller than 1 sq km")
  
  map <- map[-tiny_polygons,]
  return(map)
}

#************************************************************

set_up_maps <- function(outputdir){
  
  temppath = tempdir()
  
  Sys.setlocale("LC_CTYPE","C")
  get_maps(temppath)
  
  Sys.setlocale("LC_CTYPE", "pl_PL.utf8")
  
  all_maps <- c("jednostki_ewidencyjne", "panstwo", "wojewodztwa", "powiaty", "gminy")
  
  for (i in all_maps){
    name <- i
    map <- load_map(name, temppath)
    
    if (i == "jednostki_ewidencyjne"){                              # extracts Warsaw district data from "jednostki_ewidencyjne.shp"
      map <- subset(map, type == 8)                                 # Krakow and Lodz maps can also be found here ('type == 9')
      name <- "warszawa"
    }
     
    if (any(duplicated(map$code))){
      warning("Duplicated entries found in ", name, " data!")
      map <- remove_tiny_polygons(map)
      
      if (any(duplicated(map$code))){
        stop("Duplicates still occur, fix the data!")
      }
    }
    
    map %>%
      rmapshaper::ms_simplify(keep = 0.015) %>%                       # simplifies the polygons to reduce map size
      sp::spTransform(CRS("+init=epsg:4326")) %>%                     # converts map data to usable coordinate system
      saveRDS(paste0(outputdir, "/", name, ".rds", collapse=""))
    
    gc()
  }
  unlink(temppath)
}