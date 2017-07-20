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
    def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance_track(chars: List[Char], stack: Int): Boolean = {
          if (stack < 0) false
          else if (chars.isEmpty) stack==0
          else if (chars.head == '(') balance_track(chars.tail, stack+1)
          else if (chars.head == ')') balance_track(chars.tail, stack-1)
          else balance_track(chars.tail, stack)
        }
      balance_track(chars, 0)
    }
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChange_idx(remainder: Int, idx: Int): Int = {
        if (remainder == 0) 1
        else if (remainder < 0) 0
        else if (idx < 0 && remainder > 0) 0
        else countChange_idx(remainder, idx-1) + countChange_idx(remainder - coins(idx), idx)
      }
      countChange_idx(money, coins.length-1)
    }

}

