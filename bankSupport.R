#support

clean_num<-function(x){
  if(is.na(x)){
    return(0)
  }else{
    return(gsub(",","",x))
  }
}

numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}
