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

   private def value: Parser[String] = """[^\$]""".r
   private def propValue: Parser[String] = "[" ~> value <~ "]"
   private def ucLetter: Parser[String] = "[A-Z]".r
   private def propIdent: Parser[String] = ucLetter
   private def property: Parser[(String, List[String])] = propIdent ~ rep(propValue) ^^ { case key ~ values =>
      (key, values)
   }
   private def node: Parser[SgfNode] = ";" ~> rep(property) ^^ (_.toMap)
   private def sequence: Parser[(SgfNode, List[SgfNode])] = node ~ rep(node) ^^ { case n ~ ns => (n, ns) }
   private def gameTree: Parser[SgfTree] = "(" ~> (sequence ~ rep(gameTree)) <~ ")" ^^ {
      case (rootNode, rootSubNodes) ~ subTrees =>
         Node(rootNode, rootSubNodes.map(n => Node(n)) ++ subTrees)
   }

   def parseSgf(sgfString: String): Option[SgfTree] =
      println(s"Ordinary text: $sgfString\nParsed text: ${parseAll(gameTree, sgfString)}")
      parse(gameTree, sgfString) match
         case Success(result, _) => Some(result)
         case _: NoSuccess       => None
