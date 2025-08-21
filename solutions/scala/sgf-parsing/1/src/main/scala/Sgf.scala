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


   private def ident = "[A-Z]+".r

   private def propertyValue: Parser[String] = "[" ~> """[^]]*""".r <~ "]"

   private def property: Parser[(String, List[String])] = ident ~ rep(propertyValue) ^^ { case key ~ values =>
      (key, values)
   }
   private def node: Parser[SgfNode] = ";" ~> rep(property) ^^ { props => props.groupMapReduce(_._1)(_._2)(_ ++ _) }
   private def gameTree: Parser[SgfTree] =
      "(" ~> rep1(node) ~ rep(gameTree) <~ ")" ^^ {
         case nodesSeq ~ variations =>
            def chainNodes(nodes: Seq[SgfNode]): SgfTree = nodes match
               case Seq()       => throw new RuntimeException("Unexpected empty node sequence")
               case Seq(n)      => Node(n)
               case Seq(h, t*) => Node(h, List(chainNodes(t)))
            val mainLine = chainNodes(nodesSeq)
            def attachVariations(tree: SgfTree, vars: List[SgfTree]): SgfTree = tree match
               case Node(label, Nil) => Node(label, vars)
               case Node(label, children) =>
                  val lastChild = children.last
                  val newLastChild = attachVariations(lastChild, vars)
                  Node(label, children.init :+ newLastChild)
            attachVariations(mainLine, variations)
      }



   def parseSgf(text: String): Option[SgfTree] = {
      parseAll(gameTree, text.replaceAll("\n", " ")) match
         case Success(result, _) => println(result);Some(result)
         case Failure(msg, next)    => println(s"Parsing failed: $msg at line ${next.pos.line}, column ${next.pos.column}");None
         case Error(msg, next)      => println(s"Parsing Error: $msg at line ${next.pos.line}, column ${next.pos.column}");None
   }
