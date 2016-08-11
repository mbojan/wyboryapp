library(rmapshaper)
library(rgdal)
library(tidyr)

source("utils.R")

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
    if (grepl(pattern="\344", i)){
      file.rename(i, gsub(
        pattern="P", replacement="p", gsub(
        pattern="\344", replacement="n", i
        )))
    }
    if (grepl(pattern="\242", i)){
      file.rename(i, gsub(
        pattern="\242", replacement="o", i))
    }
  }
  
  setwd(previous_dir)
  rm(previous_dir)
}

#************************************************************

load_map <- function(name, path){
  
  map <- readOGR(paste0(path, "/", name, ".shp", collapse=""), name, use_iconv = TRUE, encoding ="Windows-1250")
  
  map@data <- map@data[, c(5, 6, 16)]
  names(map@data) <- c("code", "name", "area")
  
  if (any(duplicated(map@data$code))){
    warning("duplicates found!\n",
            print(find_duplicated(map@data, columns=1)))
  }
  
  if (name != "wojewodztwa" && name != "panstwo"){             #area data available only for these two
      map@data <- map@data[, -3]    
  }
  
  if (name == "gminy"){
    map@data <- map@data %>%
      separate("code", into=c("TERYT", "type"), sep = 6)
  }
  
  if (name == "jednostki_ewidencyjne"){
    map@data <- map@data %>%
      separate("code", into=c("TERYT", "type"), sep = "_")
  }
  
  return(map)
}
#************************************************************


path="./test"

#get_maps(path) #TO DO: locale/encoding for unzip? will the whole thing work on windows?

woj <- load_map(name="wojewodztwa", path)

gminy <- load_map(name="gminy", path)

powiaty <- load_map(name="powiaty", path)

panstwo <- load_map(name="panstwo", path)

miasta <- load_map(name="jednostki_ewidencyjne", path)
miasta <- miasta[!(miasta$TERYT %in% gminy$TERYT),]

#gminy2 <- ms_simplify(gminy, keep=0.005) #0.006 is the least possible value for wojewodztwa.shp, below it Hel Penisula disappears