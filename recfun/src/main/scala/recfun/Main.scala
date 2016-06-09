package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (edge(c,r)) 1
    else  pascal(c-1, r-1) + pascal(c, r-1)

    def edge(c: Int, r: Int): Boolean =
      c == 0 ||  c == r

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    balanceAux(chars, 0)

  def balanceAux(chars: List[Char], counter: Int): Boolean =
    if (counter < 0 || (chars.isEmpty && counter!= 0))
      false
    else if (chars.isEmpty )
      true
    else if (!parenthesis(chars.head))
      balanceAux(chars.tail, counter)
    else updateCounter(chars, counter)

  def updateCounter(chars: List[Char], counter: Int): Boolean =
    if (chars.head == '(')
      balanceAux(chars.tail, counter + 1)
    else
      balanceAux(chars.tail, counter - 1)

  def parenthesis(char: Char): Boolean =
    char == '(' || char == ')'
  balance("(((hello)there)you)(".toList)
  /**
   * Exercise 3
   */

  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

