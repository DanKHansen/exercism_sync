two_fer <- function(input) {
  if (missing(input) || length(input) == 0)
    "One for you, one for me."
  else
    sprintf("One for %s, one for me.", input)
}
