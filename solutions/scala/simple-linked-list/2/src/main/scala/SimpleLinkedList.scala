trait SimpleLinkedList[T]:
   def isEmpty: Boolean
   def value: T
   def add(item: T): SimpleLinkedList[T]
   def next: SimpleLinkedList[T]
   def reverse: SimpleLinkedList[T]
   def toSeq: Seq[T]

object SimpleLinkedList:
   def fromSeq[T](seq: Seq[T]): SimpleLinkedList[T] = LinkedList(seq.toList)
   def apply[T](ts: T*): SimpleLinkedList[T] = fromSeq[T](ts.toList)

class LinkedList[T](l: List[T]) extends SimpleLinkedList[T]:
   override def isEmpty: Boolean = l.isEmpty
   override def value: T = l.head
   override def add(item: T): SimpleLinkedList[T] = LinkedList(l.appended(item))
   override def next: SimpleLinkedList[T] = LinkedList(l.tail)
   override def reverse: SimpleLinkedList[T] = LinkedList(l.reverse)
   override def toSeq: Seq[T] = l
