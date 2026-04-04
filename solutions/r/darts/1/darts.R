library(dplyr)
score <- function(x, y) {
  r <- sqrt(x * x + y * y)
  case_when(r > 10 ~ 0, r > 5 ~ 1, r > 1 ~ 5, TRUE ~ 10)
}
