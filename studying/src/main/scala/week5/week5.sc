def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}
def insert(x: Int  , xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <=y) x :: xs else y :: insert(x,ys)
}

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("Last of empty list")
  case List(x) => x
  case y::ys => last(ys)
}

def init[T](xs: List[T]): List [T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y::ys => y :: init(ys)
}

def concat[T](xs: List[T], ys:List[T]) : List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => List()
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n + 1)

def msort[T](xs: List[T])(lt: (T,T) => Boolean ) : List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T]  =
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case(x::xs1, y::ys1) =>
          if (lt(x,y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val (fst, snd) = xs splitAt n
    merge(msort(fst)(lt), msort(snd)(lt))
  }
}

val nums = List(1,2,-4, 70, 68)
msort(nums)((x:Int, y:Int) => x < y)


def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case y :: ys => y*y :: squareList(ys)
  }

def squareListMap(xs: List[Int]): List[Int] =
  xs map (x => x*x)

def posElems(xs: List[Int]): List[Int] = xs match{
  case Nil => xs
  case y :: ys => if (y > 0) y :: posElems(ys)
  else posElems(ys)
}