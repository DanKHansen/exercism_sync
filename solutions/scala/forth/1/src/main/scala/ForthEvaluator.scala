import ForthError.ForthError

object ForthError extends Enumeration:
   type ForthError = Value
   val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value

trait ForthEvaluatorState:
   def stack: List[Int]
   override def toString: String = stack.reverse.mkString(" ")

abstract class Definition:
   def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState]

trait ForthEvaluator:
   // TODO: Implement evaluation
   def eval(text: String): Either[ForthError, ForthEvaluatorState]
