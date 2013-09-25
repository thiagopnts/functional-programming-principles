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
  def pascal(c: Int, r: Int): Int = {
    (c, r) match {
      case (c, r) if c == 0 || c == r => 1
      case (c, r) if c == 1 || (c + 1) == r => r
      case _ => pascal(r - 1, c -1) + pascal(r - 1, c -1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper(chars: List[Char], count: Int): Boolean = {
      if(count < 0) {
        false
      } else if(chars.isEmpty) {
        count == 0
      } else if(chars.head == '('){
        helper(chars.tail, count + 1)
      } else if(chars.head == ')') {
        helper(chars.tail, count - 1)
      } else {
    	helper(chars.tail, count)
      }
    }
    helper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    (money, coins) match {
      case(0, _) => 1
      case(money, _) if money < 0 => 0
      case(money, x::xs) => countChange(money - x, x::xs) + countChange(money, xs)
      case(_, _) => 0
    }
  }
}
