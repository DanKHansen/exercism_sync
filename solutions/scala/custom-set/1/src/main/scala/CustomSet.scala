object CustomSet:
   def empty(s: Set[Int]): Boolean = s.isEmpty
   def fromList(l: List[Int]): Set[Int] = l.toSet
   def member(s: Set[Int], i: Int): Boolean = s.contains(i)
   def isSubsetOf(s1: Set[Int], s2: Set[Int]) :Boolean = s1.subsetOf(s2)
   def isDisjointFrom(s1: Set[Int], s2: Set[Int]): Boolean = s1.intersect(s2).isEmpty
   def isEqual(s1: Set[Int], s2: Set[Int]): Boolean = s2 == s1
   def insert(s: Set[Int], i: Int): Set[Int] = s + i
   def intersection(s1: Set[Int], s2: Set[Int]): Set[Int] = s1.intersect(s2)
   def difference(s1: Set[Int], s2: Set[Int]): Set[Int] = s1.diff(s2)
   def union(s1: Set[Int], s2: Set[Int]): Set[Int] = s1.union(s2)