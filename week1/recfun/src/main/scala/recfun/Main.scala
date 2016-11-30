package recfun

import scala.annotation.tailrec

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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceWithSum(sum: Int, remChars: List[Char]): Boolean = {
      if (remChars.isEmpty) sum == 0
      else if (remChars.head == '(') balanceWithSum(sum + 1, remChars.tail)
      else if (remChars.head == ')')
        if (sum == 0) false else balanceWithSum(sum - 1, remChars.tail)
      else balanceWithSum(sum, remChars.tail)
    }

    balanceWithSum(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
