# This is a stub function to take two strings
# and calculate the hamming distance
hamming <- function(strand1, strand2) {
  l1 <- nchar(strand1)
  l2 <- nchar(strand2)
  if (l1 == 0 && l2 == 0) {
    return(0)
  }
  if (l1 == 0 || l2 == 0) {
    stop("Neither strand may be empty")
  }
  if (l1 != l2) {
    stop("Strands must be of equal length")
  }

  sum(strsplit(strand1, "")[[1]] != strsplit(strand2, "")[[1]])
}
