#checked if there are any same name/surname combinations in the 2015 data

#found 16 cases (8 pairs)
#none of these pairs occur in the same precinct
#thus: we can safely identify a person by a combination of 'Name', 'Surname' and election district number

source("utils.R")

require(XLConnect)
require(dplyr)

path <- "/home/ev/wybory/raw_data/wyniki/2015/kandsejm2015-10-19-10-00.xls"

kandydaci <- XLConnect::readWorksheetFromFile(path, sheet=1, header=TRUE) %>%
  select(-Zawód, -Płeć, -Miejsce.zam., -Należy.do.partii.politycznej)

#clean irregular characters just in case
kandydaci$Nazwisko <- rmv_polish_char(kandydaci$Nazwisko)
kandydaci$Nazwisko <- gsub(pattern="\\.", replacement=" ", kandydaci$Nazwisko, fixed=FALSE)
kandydaci$Nazwisko <- gsub(pattern="-", replacement=" ", kandydaci$Nazwisko, fixed=FALSE)

#finds all values that already ocurred, from beginning to end
dup <- kandydaci %>%
  select(Imiona, Nazwisko) %>%
  duplicated()

#now find the repeated values from end to beginning, to get the first ocurrences
dup2 <- kandydaci[nrow(kandydaci):1,] %>%
  select(Imiona, Nazwisko) %>%
  duplicated()

#list all duplicates
duplikaty <- rbind(
  kandydaci %>%
      filter(dup),
  kandydaci[nrow(kandydaci):1,] %>%
      filter(dup2)
)