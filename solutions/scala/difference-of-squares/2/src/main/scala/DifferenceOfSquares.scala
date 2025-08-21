object DifferenceOfSquares:
   def sumOfSquares(n: Int): Int = (1 to n).map(i => i * i).sum
   def squareOfSum(n: Int): Int = (1 to n).sum * (1 to n).sum
   def differenceOfSquares(n: Int): Int = squareOfSum(n) - sumOfSquares(n)
