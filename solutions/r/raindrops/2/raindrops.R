raindrops <- function(number) {
  sounds <- c(Pling = 3, Plang = 5, Plong = 7)
  res <- paste0(names(sounds[number %% sounds == 0]), collapse = "")
  if (nchar(res) == 0) as.character(number) else res
}
