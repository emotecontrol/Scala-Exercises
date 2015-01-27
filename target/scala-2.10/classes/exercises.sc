// P01 Find the last element of a list
def last(list: List[Any]): Any = list match {
  case x::Nil => x
  case x::xs => last(xs)
  case _ => Nil
}
last(List(1,1,2,3,5,8))
// P02 Find the last-but-one element of a list
def penultimate(list: List[Any]): Any = list match{
  case x::y::Nil => x
  case x::y::xs => penultimate(y::xs)
  case _ => Nil
}
penultimate(List(1,1,2,3,5,8))
// P03 Find the Kth element of a list
def nth(n:Int, list:List[Any]): Any = {
  if (n==0) list.head
  else nth(n-1, list.tail)
}
nth(2,List(1,1,2,3,5,8))
// P04 Find the number of elements of a list
def length(list: List[Any]): Int = {
  def counter(i: Int, list:List[Any]): Int = {
    if (list == Nil) i
    else counter (i+1, list.tail)
  }
  counter(0, list)
}
length(List(1,1,2,3,5,8))
// P05 Reverse a list
def reverse(list: List[Any]): List[Any] = {
  def collect(agg: List[Any], source: List[Any]):List[Any] = {
    if (source == Nil) agg
    else collect(source.head::agg, source.tail)
  }
  collect(Nil, list)
}
reverse(List(1,1,2,3,5,8))

// P06 Find out whether list is a palindrome
def isPalindrome(list: List[Any]): Boolean = {
  def trimFirstLast(trim: List[Any]): List[Any] = {
    reverse(reverse(trim).tail).tail
  }
  if (list == Nil || length(list) == 1) true
  else if (list.head == last(list)) isPalindrome(trimFirstLast(list))
  else false
}
isPalindrome(List(1,2,3,2,1))
isPalindrome(List(1,2,2,1))
isPalindrome(List(1,2,3,1))
isPalindrome(List(1,2,1))
isPalindrome(Nil)

// P07 Flatten a nested list structure
def flatten(list: List[Any]): List[Any] = {
  def output(l: Any): List[Any] = l match {
    case x::Nil => output(x)
    case x::xs => output(x) ++ output(xs)
    case x => List(x)
  }
  output(list)
}
flatten(List(List(1,1),2,List(3,List(5,8))))

// P08 Eliminate consecutive duplicates of list elements
def compress(list: List[Any]): List[Any] = {
  def reduce(l: List[Any], newList: List[Any]):List[Any] = {
    if (l == Nil) newList
    else if (newList == Nil) reduce(l.tail, List(l.head))
    else if (last(newList) == l.head) reduce(l.tail, newList)
    else reduce(l.tail, newList:+l.head)
  }
  reduce(list, Nil)
}
compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

// P09 Pack consecutive duplicates of list elements into sublists
def pack(list: List[Any]): List[List[Any]] = {
  // package one letter's worth as (trimmedList, oneLetterList)
  def snip(l: List[Any], agg: List[Any]): (List[Any], List[Any]) = {
    if (l == Nil) (l, agg)
    else if (agg == Nil) snip (l.tail, agg :+ l.head)
    else if (l.head == agg.head) snip(l.tail, agg :+ l.head )
    else (l, agg)
  }
  def makeSnips(l: List[Any], agg: List[List[Any]]):List[List[Any]] = {
    if (l == Nil) agg
    else {
      val trims: (List[Any], List[Any]) = snip(l, Nil)
      makeSnips(trims._1, agg :+ trims._2)
    }
  }
  makeSnips(list, Nil)
}
pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

// P10 Run-length encoding of a list
def encode(list: List[Any]): List[(Int, Any)] = {
  val packed = pack(list)
  def repack(l: List[List[Any]], agg:List[(Int,Any)]): List[(Int, Any)] = {
    if (l == Nil) agg
    else repack (l.tail, agg :+ (length(l.head),l.head.head))
  }
  repack(packed, Nil)
}
encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

// P11 Modified run-length encoding
def encodeModified(list: List[Any]): List[Any] = {
  val e = encode(list)
  def encodeM(l: List[(Int,Any)], agg:List[Any]): List[Any] = {
    if (l == Nil) agg
    else if (l.head._1 == 1) encodeM(l.tail, agg :+ l.head._2)
    else encodeM(l.tail, agg :+ l.head)
  }
  encodeM(e, Nil)
}
encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

// P12 Decode a run-length encoded list
def decode(list: List[(Int, Any)]): List[Any] = {
  def unpack(l: List[(Int, Any)], agg: List[Any]): List[Any] = {
    if (l == Nil) agg
    else unpack (l.tail, agg :+ spread(l.head._2, l.head._1, Nil))
  }
  def spread(l: Any, i: Int, agg: List[Any]): List[Any] = {
    if (i == 0) agg
    else spread(l, i-1, agg :+ l)
  }
  flatten(unpack(list, Nil))
}
decode(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

// P13 Run-length encoding of a list (direct solution)
def encodeDirect(list: List[Any]): List[(Int, Any)] = {
  def comprise(l: List[Any], agg: List[(Int, Any)], currAgg: (Option[Int], Option[Any])):List[(Int,Any)] = {
    if (l == Nil) agg:+(currAgg._1.get, currAgg._2.get)
    else if (currAgg._2 == None) comprise(l.tail, agg, (Some(1), Some(l.head)))
    else if (currAgg._2.get != l.head) comprise(l.tail,
      agg :+ (currAgg._1.get, currAgg._2.get), (Some(1), Some(l.head)))
    else comprise (l.tail, agg, (Some(currAgg._1.get + 1), currAgg._2))
  }
  comprise(list, Nil, (None, None))
}
encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

// P14 Duplicate the elements of a list
def duplicate(list: List[Any]): List[Any] = {
  def double(l: List[Any], agg: List[Any]): List[Any] = {
    if (l == Nil) agg
    else double (l.tail, agg :+ l.head :+ l.head)
  }
  double(list, Nil)
}
duplicate(List('a, 'b, 'c, 'c, 'd))

// P15 Duplicate the elements of a list a given number of times
def duplicateN(n: Int, list: List[Any]): List[Any] = {
  def multiply(n: Int, m: Int, l: List[Any], agg: List[Any]): List[Any] = {
    if (l == Nil) agg
    else if (m == 0) multiply(n, n, l.tail, agg)
    else multiply(n, m-1, l, agg :+ l.head)
  }
  multiply(n, n, list, Nil)
}
duplicateN(3, List('a, 'b, 'c, 'c, 'd))

// P16 Drop every Nth element from a list
def drop(n: Int, list: List[Any]): List[Any] = {
  def remove(count: Int, l: List[Any], agg: List[Any]):List[Any] = count match {
    case x if x == list.length + 1 => agg
    case x if x%n==0 => remove(count + 1, l.tail, agg)
    case _ => remove(count + 1, l.tail, agg :+ l.head)
  }
  remove(1, list, Nil)
}
drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

// P17 Split a list into two parts
def split(n: Int, list: List[Symbol]): (List[Symbol], List[Symbol]) = {
  def splitHelper(count: Int, l: List[Symbol], agg: (List[Symbol], List[Symbol])): (List[Symbol], List[Symbol]) = count match {
    case x if x == list.length => agg
    case x if x < n => splitHelper(count + 1, l.tail, (agg._1 :+ l.head, agg._2))
    case _ => splitHelper(count + 1, l.tail, (agg._1, agg._2 :+ l.head))
  }
  splitHelper(0, list, (Nil, Nil))
}
split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

// P18 Extract a slice from a list
def slice(n: Int, m: Int, list: List[Symbol]): List[Symbol] = {
  def sliceHelper(count: Int, l: List[Symbol], agg: List[Symbol]): List[Symbol] = count match {
    case x if x == n + m => agg
    case x if x >= n && x < m => sliceHelper(count + 1, l.tail, agg :+ l.head)
    case _ => sliceHelper(count + 1, l.tail, agg)
  }
  sliceHelper(0, list, Nil)
}
slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

// P19 Rotate a list N places to the left
def rotate(n: Int, list: List[Symbol]): List[Symbol] = {
  def rotateHelper(count: Int, l: List[Symbol], agg: List[Symbol]): List[Symbol] = count match {
    case x if x == n && n >= 0 || x == list.length + n && n < 0 => l ++ agg
    case _ => rotateHelper(count + 1, l.tail, agg :+ l.head)
  }
  rotateHelper(0, list, Nil)
}
rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
rotate(0, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

// P20 Remove the Kth element from a list
def removeAt(n: Int, list: List[Symbol]): (List[Symbol], Symbol) = {
  val splitList = split(n, list)
  (splitList._1 ++ splitList._2.tail, splitList._2.head)
}
removeAt(1, List('a, 'b, 'c, 'd))

// P21 Insert an element at a given position into a list
def insertAt(add: Symbol, n: Int, list: List[Symbol]): List[Symbol] = {
  val splitList = split(n, list)
  splitList._1 ++ List(add) ++ splitList._2
}
insertAt('new, 1, List('a, 'b, 'c, 'd))

// P22 Create a list containing all integers within a given range
def range(n: Int, m: Int): List[Int] = {
  def rangeHelper(count: Int, agg: List[Int]):List[Int] = count match {
    case x if x > m => agg
    case x => rangeHelper(count + 1, agg :+ x)
  }
  rangeHelper(n, Nil)
}
range(4,9)

// P23 Extract a given number of randomly selected elements from a list
def randomSelect(n: Int, list: List[Symbol]) = {
  val r = new scala.util.Random()
  def randomHelper(count: Int, l: List[Symbol], agg: List[Symbol]): List[Symbol] = {
    if (count >= n) agg
    else {
      val extract = removeAt(r.nextInt(l.length), l)
      randomHelper(count + 1, extract._1, agg :+ extract._2)
    }
  }
  randomHelper(0, list, Nil)
}
randomSelect(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h))

