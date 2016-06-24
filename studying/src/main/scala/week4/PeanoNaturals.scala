package week4

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat{
  def isZero = true
  def predecesor = throw new Error("0.predecesor")
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new Error("Negative Number")
}
class Succ(n: Nat) extends Nat{
  def isZero = false
  def predecesor = n
  def + (that: Nat) = new Succ(n + that)
  def - (that: Nat) = if (that.isZero) this else n - that.predecessor
}
