class EmptyBufferException() extends Exception {}

class FullBufferException() extends Exception {}

class CircularBuffer(val capacity: Int):
   private val b = scala.collection.mutable.Queue[Int]()

   def write(value: Int): Unit = if capacity == b.size then throw FullBufferException() else b.append(value)

   def read(): Int = if b.isEmpty then throw EmptyBufferException() else b.dequeue()

   def overwrite(value: Int): Unit =
      if capacity == b.size then read() else (); write(value)

   def clear(): Unit = b.clear()
