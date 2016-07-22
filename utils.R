require(XLConnect)

get_xls_data <- function(file_path, is_head=TRUE){
  
  wb <- loadWorkbook(file_path)
  dataset <- readWorksheet(wb, sheet=1, header=is_head)
  rm(wb)
  
  return(dataset)
}

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