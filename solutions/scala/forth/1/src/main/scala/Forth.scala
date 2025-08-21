import scala.annotation.tailrec
type Result = Either[ForthError.Value, ForthEvaluatorState]

case class State(stack: List[Int]) extends ForthEvaluatorState:
   override def toString: String = super.toString

class Forth extends ForthEvaluator:
   def eval(text: String): Result =
      execute(text.toLowerCase.split(" ").toList, Right(State(Nil)))

sealed trait Operation
case object Add extends Operation
case object Sub extends Operation
case object Mul extends Operation
case object Div extends Operation
case object Dup extends Operation
case object Swap extends Operation
case object Over extends Operation
case object Drop extends Operation

object Def extends Definition:
   case class Num(num: Int) extends Definition:
      override def evaluate(state: Result): Result =
         state match
            case Left(value) => Left(value)
            case Right(st)   => Right(State(num :: st.stack))
   def eval(state: Result, op: Operation): Result =
      state match
         case Left(error) => Left(error)
         case Right(st)   =>
            st.stack match
               case Nil                    => Left(ForthError.StackUnderflow)
               case head :: follow :: rest =>
                  op match
                     case Add  => Right(State(head + follow :: rest))
                     case Sub  => Right(State(follow - head :: rest))
                     case Mul  => Right(State(follow * head :: rest))
                     case Div  =>
                        if head == 0 then Left(ForthError.DivisionByZero)
                        else Right(State(follow / head :: rest))
                     case Dup  => Right(State(head :: head :: follow :: rest))
                     case Swap => Right(State(follow :: head :: rest))
                     case Over => Right(State(follow :: head :: follow :: rest))
                     case Drop => Right(State(follow :: rest))
               case head :: Nil            =>
                  op match
                     case Dup  => Right(State(head :: head :: Nil))
                     case Drop => Right(State(Nil))
                     case _    => Left(ForthError.StackUnderflow)

   override def evaluate(state: Result): Result =
      Left(ForthError.InvalidWord)

def udf(text: List[String], st: Result, dict: Map[String, String]): Result =
   val ws = text.map(_.toLowerCase).takeWhile(_ != ";").drop(1)
   if ws.isEmpty || ws.head.forall(_.isDigit) then Left(ForthError.InvalidWord)
   else
      val newDict = dict + (ws.head -> ws.tail.map(w => dict.getOrElse(w, w)).mkString(" "))
      val newWS = text.diff(ws).drop(2)
      execute(newWS, st, newDict)

@tailrec
def execute(ws: List[String], st: Result, dict: Map[String, String] = Map().empty): Result =
   ws.headOption match
      case None                           => st
      case Some(n) if n.forall(_.isDigit) => execute(ws.tail, Def.Num(n.toInt).evaluate(st), dict)
      case Some(w)                        =>
         dict.get(w) match
            case Some(s) => execute(s.split(" ").toList ::: ws.tail, st, dict)
            case None    =>
               w match
                  case "+"    => execute(ws.tail, Def.eval(st, Add), dict)
                  case "-"    => execute(ws.tail, Def.eval(st, Sub), dict)
                  case "*"    => execute(ws.tail, Def.eval(st, Mul), dict)
                  case "/"    => execute(ws.tail, Def.eval(st, Div), dict)
                  case "dup"  => execute(ws.tail, Def.eval(st, Dup), dict)
                  case "drop" => execute(ws.tail, Def.eval(st, Drop), dict)
                  case "swap" => execute(ws.tail, Def.eval(st, Swap), dict)
                  case "over" => execute(ws.tail, Def.eval(st, Over), dict)
                  case ":"    => udf(ws, st, dict)
                  case _      => Left(ForthError.UnknownWord)
