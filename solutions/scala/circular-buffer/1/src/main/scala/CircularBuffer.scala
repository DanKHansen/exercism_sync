import scala.collection.mutable.ListBuffer

class EmptyBufferException() extends Exception {}

class FullBufferException() extends Exception {}

class CircularBuffer(val capacity: Int):
   private val b = ListBuffer.empty[Int]

   def write(value: Int): Unit = if capacity == b.size then throw new FullBufferException()
   else b.append(value)

   def read(): Int = if b.isEmpty then throw new EmptyBufferException()
   else
      val v = b.head
      b.remove(0)
      v

   def overwrite(value: Int): Unit =
      if capacity <= b.size then b.remove(0) else ()
      b.append(value)

   def clear(): Unit = b.clear()
