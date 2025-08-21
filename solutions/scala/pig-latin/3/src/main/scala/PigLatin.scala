object PigLatin:
   private val cs = ('a' to 'z').diff(Seq('a', 'e', 'i', 'o', 'u'))
   def translate(s: String): String = s.split(" ").map {
        case w if !w.headOption.exists(cs.contains) || w.startsWith("xr") || w.startsWith("yt") => w + "ay"
        case s @ w                                                                              =>
           val cc = w.iterator.takeWhile(c => cs.contains(c) && (c != 'y' || w.startsWith("y"))).toList.mkString
           s.replaceFirst(cc, "") match
              case s"u$s" if cc.endsWith("q") => s"$s${cc}uay"
              case s                          => s"$s${cc}ay"
      }.mkString(" ")
