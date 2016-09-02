
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  x <- paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
  
  s <- strsplit(x, "-")[[1]]
  x <- paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
  return(x)
}
