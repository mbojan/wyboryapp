library(rmapshaper)
library(rgdal)

load_and_simplify <- function(name, path, keeps=0.06){ #0.06 is the least possible value for wojewodztwa.shp, below it Hel Penisula disappears
  map <- readOGR(paste0(path, "/", name, ".shp", collapse=""), name)
  map <- ms_simplify(map, keep=keeps)
  
  map@data <- map@data[, c(5, 6, 16)]
  names(map@data) <- c("kod", "nazwa", "powierzchnia")
  #map@data$nazwa<-iconv(map@data$nazwa, "UTF-8", "Windows-1250")
  
  return(map)
}

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
    if (grepl(pattern="ewidencyjne", i)){
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

get_maps(path="./visual/data/maps") #TO DO: is setlocale necessary? will the whole thing work on windows?

woj <- load_and_simplify(name="wojewodztwa", path="./visual/data/maps")
#gminy <- load_and_simplify(name="gminy", path="./visual/data/maps")
#powiaty <- load_and_simplify(name="powiaty", path="./visual/data/maps")
panstwo <- load_and_simplify(name="panstwo", path="./visual/data/maps")
