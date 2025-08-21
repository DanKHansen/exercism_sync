import scala.util.parsing.combinator.*
object Sgf extends RegexParsers:

   private type Tree[A] = Node[A] // to separate the type from the constructor, cf. Haskell's Data.Tree
   private type Forest[A] = List[Tree[A]]
   case class Node[A](rootLabel: A, subForest: Forest[A] = Nil)

   // A tree of nodes.
   private type SgfTree = Tree[SgfNode]

   // A node is a property list, each key can only occur once.
   // Keys may have multiple values associated with them.
   private type SgfNode = Map[String, List[String]]

   private def value: Parser[String] = "(?:\\\\\\]|[^\\]])*".r ^^ {
      _.replace("\\\\", "\\")
         .replace("\t", " ")
         .replace("\\\n", "")
         .replace("\n", " ")
         .replace("\\]", "]")
   }
   private def propVal: Parser[String] = "[" ~> value <~ "]"
   private def ucLet: Parser[String] = "[A-Z]".r
   private def propId: Parser[String] = ucLet
   private def property: Parser[(String, List[String])] = propId ~ propVal.* ^^ { case k ~ vs => (k, vs) }
   private def node: Parser[SgfNode] = ";" ~> property.* ^^ { _.toMap }
   private def seq: Parser[(SgfNode, List[SgfNode])] = node ~ node.* ^^ { case n ~ ns => (n, ns) }
   private def gT: Parser[SgfTree] = "(" ~> seq ~ gT.* <~ ")" ^^ { case s ~ g => Node(s._1, s._2.map(Node(_)) ++ g) }

   def parseSgf(sgfString: String): Option[SgfTree] =
      parse(gT, sgfString) match
         case Success(result, _) => Some(result)
         case _: NoSuccess       => None
