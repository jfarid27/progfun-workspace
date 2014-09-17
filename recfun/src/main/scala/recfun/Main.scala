package recfun
import common._

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
    if (c == 0 & r == 0) 1
    else {
      if (c == 0 || c == r) 1
      else {
        pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }

  /**
   * Exercise 2
   */

  def balanceRec(balance: Int, chars: List[Char]): Boolean =

    if (chars.isEmpty) {
      if (balance == 0) {
        return true
      } else {
        return false
      }
    } else if (balance < 0) {
      return false
    }
    else {
      if (chars.head.toString() == "(") {
        return balanceRec(balance + 1, chars.tail)
      } else if (chars.head.toString() == ")") {
        return balanceRec(balance - 1, chars.tail)
      } else {
        return balanceRec(balance, chars.tail)
      }
    }

  def balance(chars: List[Char]): Boolean = balanceRec(0, chars)

  /**
   * Exercise 3
   */

  def countChange(money: Int, coins: List[Int]): Int = {

    for {
      coin <- coins
    } yield countChange(money, List(coin))
  }



}
