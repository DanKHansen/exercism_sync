case class Deque[T]():
   private var stack: List[T] = List.empty[T]

   def push(elem: T): Unit =
      stack = stack.prepended(elem)

   def pop: Option[T] =
      val ret = stack.headOption
      stack = stack.tail
      ret

   def shift: Option[T] =
      val ret = stack.lastOption
      stack = stack.init
      ret

   def unshift(elem: T): Unit =
      stack = stack.appended(elem)
