 object rationals{
   var x = new Rational(1,3)
   var y = new Rational(5,7)
   var z = new Rational(3,2)
   x.numer
   x.denom

   x - y - z

   y + y
   x < y
   x.max(y)
   var w = new Rational(2)

   class Rational(x: Int, y: Int){
     require(y != 0, "Denominator must not be 0")

     def this(x: Int) = this(x,1)

     private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

     def numer = x
     def denom = y

     def < (that: Rational) = numer * that.denom < that.numer * denom

     def max(that: Rational) = if (this < that) that else this

     def unary_- : Rational = new Rational(-numer, denom)

     def + (that: Rational) =
       new Rational(
         numer* that.denom + that.numer*denom,
         denom*that.denom
       )
     def - (that:Rational) = this + -that

     override def toString = {
       val g = gcd(numer,denom)
       numer/g + "/" + denom/g
     }
   }
 }


 // a + b ^? c ?^ d less a ==> b | c