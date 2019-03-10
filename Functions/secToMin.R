secToMin <- function(seconds) {
  
  min <- floor(seconds / 60)
  secs <- round(seconds%%60,1)
  
  output <- ifelse(min == 0
                   , paste(secs,"sec", sep = "")
                   , paste(min, "min ",round(secs,0), "sec", sep = ""))
  output
}