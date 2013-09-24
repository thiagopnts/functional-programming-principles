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
    if (c == 0 || c == r) {
      return 1
    } else if(c == 1 || (c + 1) == r) {
      return r
    } else {
     return pascal(r - 1, c - 1) + pascal(r - 1, c) 
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper(chars: List[Char], count: Int): Boolean = {
      if(count < 0) {
        return false
      } else if(chars.isEmpty) {
        return count == 0
      } else if(chars.head == '('){
        return helper(chars.tail, count + 1)
      } else if(chars.head == ')') {
        return helper(chars.tail, count - 1)
      } else {
    	return helper(chars.tail, count)
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
