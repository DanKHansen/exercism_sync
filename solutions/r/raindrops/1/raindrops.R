raindrops <- function(number) {
  factors <- c(3, 5, 7)
  sounds <- c("Pling", "Plang", "Plong")
  res <- paste0(sounds[number %% factors == 0], collapse = "")
  if (nchar(res) == 0) as.character(number) else res
}