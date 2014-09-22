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

  def countChangeRec(money:Int, denominations:List[Int]): Set[List[Int]] = {

    if (denominations.isEmpty){
      return Set()
    }

    //If money == denominations.head
    if (money == denominations.head){
      //if denominatons.tail.isEmpty
      if (denominations.tail.isEmpty){
        //Return set with denomination only
        return Set(List(denominations.head))

      //else tail isn't empty
      } else {
        return Set(List(denominations.head)) ++ countChangeRec(money, denominations.tail)
        //return 1 + countChangeRec(money, denominations.tail)
      }


    } else if (money > denominations.head) {
      //build sets with only denominations.head money amount
      val head = countChangeRec(denominations.head, denominations)

      //build sets with money - denominations.head money amount
      val rest = countChangeRec(money-denominations.head, denominations)

      //merge sets and remove duplicates

      val merge = for {
        lead <- head
        rear <- rest
      } yield (lead ++ rear).sorted

      //return merged sets
      return merge

    } else { // (money < denominations.head) {
      return countChangeRec(money, denominations.tail)
    }

    return Set()

  }

  def countChange(money: Int, coins: List[Int]): Int = {


    val sets = countChangeRec(money, coins.sorted.reverse)

    if (!sets.isEmpty){

      val items = for {
        list <- sets
      } yield list.sum

      return sets.size
    }

    return 0
  }




}
