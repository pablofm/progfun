abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

def times(chars: List[Char]): List[(Char, Int)] = {
  def timesAcc(chars: List[Char], acc: List[(Char,Int)]): List[(Char,Int)] = chars match {
    case Nil => acc
    case y :: ys => timesAcc(ys, insertTimes(y, acc))
  }
  def insertTimes(x: Char, acc: List[(Char, Int)]): List[(Char, Int)] = acc match {
    case Nil => (x, 1) :: Nil
    case pair :: ys => if (pair._1 == x) (x, pair._2 + 1) :: ys else pair :: insertTimes(x, ys)
  }
  timesAcc(chars, Nil)
}

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
  def createListLeaf(freqs: List[(Char, Int)]) : List[Leaf] =
    if (freqs.isEmpty) Nil
    else new Leaf(freqs.head._1, freqs.head._2) :: createListLeaf(freqs.tail)

  def isort(xs: List[(Char, Int)]) : List[(Char, Int)] = xs match {
    case Nil => Nil
    case y :: ys => insert(y, isort(ys))
  }
  def insert(pair: (Char, Int), xs: List[(Char, Int)]) : List[(Char, Int)] = xs match {
    case Nil => List(pair)
    case y :: ys => if (pair._2 <= y._2) pair :: xs else y :: insert(pair, ys)
  }

  createListLeaf(isort(freqs))

}


val list_1 = List('a')
val list_2 = List('a', 'b')
val list_3 = List('a', 'a')
val list_4 = List('a', 'b', 'a')
val list_5 = List('a', 'a', 'a')


times(list_1)
times(list_2)
times(list_3)
times(list_4)
times(list_5)

makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))
list_1.size
