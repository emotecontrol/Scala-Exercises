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
encodeModified