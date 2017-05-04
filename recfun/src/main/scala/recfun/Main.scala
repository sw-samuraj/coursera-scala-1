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
    def pascal(c: Int, r: Int): Int = {
      def loop(prev: Int, col: Int): Int = {
        def res = prev * r / col

        if (c == col) res
        else loop(res, col + 1)
      }

      if (c == 0) 1
      else loop(1, 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def count(c: Char, parens: List[Char]): List[Char] = {
        if (c == '(') parens :+ c
        else if (c == ')' && parens.isEmpty) parens :+ c
        else if (c == ')') parens.reverse.tail.reverse
        else parens
      }

      def loop(parens: List[Char], tail: List[Char]): Boolean = {
        if (tail.isEmpty) parens.isEmpty
        else loop(count(tail.head, parens), tail.tail)
      }

      if (!chars.contains('(')) false
      else loop(List[Char](), chars)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0) 0
      else if (coins.isEmpty)
        if (money == 0) 1 else 0
      else
        countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
