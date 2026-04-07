raindrops <- function(number) {
  res <- c("Pling", "Plang", "Plong")[number %% c(3, 5, 7) == 0]
  if (length(res)) paste(res, collapse = "") else paste(number)
}
