import math.abs

object week2{
  def sum(f: Int => Int)(a:Int, b:Int) : Int =
    if (a > b) 0
    else f(a) + sum(f)(a+1, b)

  def product1(f: Int => Int)(a:Int, b:Int) : Int =
    if (a > b) 1
    else f(a) * product1(f)(a+1, b)

  def product2(f: Int => Int)(a:Int, b:Int) : Int = mapReduce(f, (x,y) => x*y, 1)(a,b)
    product2(x=>x*x)(3,4)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int) (a: Int, b:Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero) (a+1, b))

  def factorial(n:Int): Int = product2(x => x) (1,n)
  factorial(5)

  // Finding fixed points

  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) = abs((x-y) / x)/ x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      println(next)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  fixedPoint(x => 1+ x/2)(1)

  def averageDamp(f: Double => Double) (x: Double) = (x + f(x)) / 2
  def sqrt(x: Double) = fixedPoint(averageDamp((y => x / y))(1.0)
}
