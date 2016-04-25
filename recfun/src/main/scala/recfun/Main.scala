package recfun
import common._

object Main {
  def main(args: Array[String]): Unit = {
    println(countChange(300,List(5,10,20,50,100,200,500)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c > 0 && r > 0) {
      val left = pascal(c - 1, r - 1)
      val right = pascal(c, r - 1)
      left + right
    }
    else {
      if (c < 0 || c > r) 0 else 1
    }
  }

  def pascalPrint() = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char], open:Int =0): Boolean = {

    if(chars.isEmpty) {
      open == 0
    } else if (open == 0 && chars.head == ')') {
      false
    } else {
      chars.head match {
        case '(' => balance(chars.tail, open + 1)
        case ')' => balance(chars.tail, open - 1)
        case _ => balance(chars.tail, open)
      }
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countRec(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money < 0) {
        0
      }
      else if(money - coins.head == 0) {
        1
      } else {
        countRec(money, coins.tail) + countRec(money - coins.head, coins)
      }
    }
    countRec(money, coins.sortBy(x => x))
  }
}
