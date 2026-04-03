leap <- function(year) {
  
  div400 <- year %% 400 == 0 
  div4 <- year %% 4 == 0 
  div100 <- year %% 100 == 0
  
  div400 | div4 & !div100

}


