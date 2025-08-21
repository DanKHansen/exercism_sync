case class ComplexNumber(real: Double = 0, imaginary: Double = 0):
   val (r, i) = (real, imaginary)
   def +(c: ComplexNumber): ComplexNumber = ComplexNumber(r + c.r, i + c.i)
   def -(c: ComplexNumber): ComplexNumber = ComplexNumber(r - c.r, i - c.i)
   def *(c: ComplexNumber): ComplexNumber = ComplexNumber(r * c.r - i * c.i, i * c.r + r * c.i)
   def /(c: ComplexNumber): ComplexNumber = ComplexNumber((r * c.r + i * c.i) / (c.r * c.r + c.i * c.i), (i * c.r - r * c.i) / (c.r * c.r + c.i * c.i))
   def abs: Double = math.hypot(r, i)
   def conjugate: ComplexNumber = ComplexNumber(r, -i)
object ComplexNumber:
   def exp(c: ComplexNumber): ComplexNumber = ComplexNumber(math.exp(c.r) * math.cos(c.i), math.exp(c.r) * math.sin(c.i))