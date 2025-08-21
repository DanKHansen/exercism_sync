import scala.util.Random
class Cipher(cif: Option[String]):

   val key: String = cif
      .map(s =>
         require(s.forall(_.isLower) && s.nonEmpty)
         s)
      .getOrElse((0 until 100).map(_ => Random.between(97, 122).toChar).mkString)

   private val cs = 'a' to 'z'

   private def shift(c: Char, k: Char): Char =
      cs((c + k - 2 * 'a') % 26)

   private def revShift(c: Char, k: Char): Char =
      cs((c - k + 26) % 26)

   def encode(s: String): String =
      s.zip(key).map(shift).mkString

   def decode(s: String): String =
      s.zip(key).map(revShift).mkString
