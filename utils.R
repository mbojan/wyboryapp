library(dplyr)

rmv_polish_char <- function(dataset){
  
  dataset<- toupper(dataset)
  
  dataset<- gsub(pattern="Ł", replacement="L", dataset, fixed=FALSE)
  
  dataset<- gsub(pattern="Ń", replacement="N", dataset, fixed=FALSE)
  
  dataset<- gsub(pattern="Ą", replacement="A", dataset, fixed=FALSE)
  
  dataset<- gsub(pattern="Ę", replacement="E", dataset, fixed=FALSE)
  
  dataset<- gsub(pattern="Ś", replacement="S", dataset, fixed=FALSE)
  
  dataset<- gsub(pattern="Ż", replacement="Z", dataset, fixed=FALSE)
  
  dataset<- gsub(pattern="Ź", replacement="Z", dataset, fixed=FALSE)
  
  dataset<- gsub(pattern="Ć", replacement="C", dataset, fixed=FALSE)
  
  dataset<- gsub(pattern="Ó", replacement="O", dataset, fixed=FALSE)
  
  return(dataset)
}

find_duplicated <- function(data, columns){
  
  dup <- data %>%
    select(columns) %>%
    duplicated()
  
  dup2 <- data[nrow(data):1,] %>%
    select(columns) %>%
    duplicated()
  
  found <- rbind(
    data %>%
      filter(dup),
    data[nrow(data):1,] %>%
      filter(dup2)
  )
  
  rm(dup, dup2)
  
  return(found)
}