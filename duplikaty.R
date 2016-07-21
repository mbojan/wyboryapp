#checked if there are any same name/surname combinations in the 2015 data

#found 16 cases (8 pairs)
#none of these pairs occur in the same precinct
#thus: we can safely identify a person by a combination of 'Name', 'Surname' and election district number

#uploading this anyway in case it's useful later on

source("utils.R")

require(dplyr)

path <- "/home/ev/wybory/raw_data/wyniki/2015/kandsejm2015-10-19-10-00.xls"

kandydaci <- get_xls_data(path) %>%
  select(-Zawód, -Płeć, -Miejsce.zam., -Należy.do.partii.politycznej)

#clean irregular characters just in case
kandydaci$Nazwisko <- rmv_polish_char(kandydaci$Nazwisko)

kandydaci$Nazwisko <- gsub(pattern="\\.", replacement=" ", kandydaci$Nazwisko, fixed=FALSE)
kandydaci$Nazwisko <- gsub(pattern="-", replacement=" ", kandydaci$Nazwisko, fixed=FALSE)

#get list of surnames that occur more than once and drop the rest
ile <- count(kandydaci, Nazwisko)
ile <- filter(ile, n!=1)
kandydaci <- kandydaci[(kandydaci$Nazwisko %in% ile$Nazwisko),]

#for every repeated surname, filter all occurences and check if the names match there as well
duplikaty <- data.frame()

for (i in 1:nrow(ile)){
  temp <- filter(kandydaci, toString(ile[i, 1]) == Nazwisko)
  if (anyDuplicated(temp$Imiona)){
    ileimion <- count(temp, Imiona)
    ileimion <- filter(ileimion, n!=1)
    print(ileimion) #DEBUG!
    print(temp[1,5]) #DEBUG!
    for (j in 1:nrow(ileimion)){
      temp2 <- filter(temp, toString(ileimion[j, 1]) == Imiona)
      duplikaty <- rbind(duplikaty, temp2)
    }
    rm(temp2)
    rm(ileimion)
  }
  rm(temp)
}
