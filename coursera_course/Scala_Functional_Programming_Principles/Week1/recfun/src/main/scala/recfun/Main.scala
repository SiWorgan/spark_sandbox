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
  def pascal(c: Int, r: Int): Int = if (c < 0 || r < 0 || c > r) {
    0
  } else if (r == 0 || c == 0 || c == r) {
    1
  } else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def stack_balance(open: Int, chars: List[Char]): Boolean = if (chars.isEmpty && open==0)
        true
      else if (chars.isEmpty)
        false
      else if (open < 0)
        false
      else if (chars.head == '(')
        stack_balance(open + 1, chars.tail)
      else if (chars.head == ')')
        stack_balance(open - 1, chars.tail)
      else
        stack_balance(open, chars.tail)

      stack_balance(0,chars)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def generate_children(remainder: Int, coins: List[Int]): List[Int] = {
        if ( coins.length > 1 )
          List(remainder-coins.head) ++ generate_children(remainder, coins.tail)
        else
          List(remainder-coins.head)
      }

      def apply_countChange(run_tot: Int, coins: List[Int], candidates: List[Int]): Int = {
        if (candidates.isEmpty || coins.isEmpty)
          run_tot
        else if (candidates.head == 0)
          if (candidates.length > 1)
            1 + run_tot + apply_countChange(run_tot, coins.tail, candidates.tail)
          else
            1 + run_tot
        else if (candidates.head < coins.min)
          if (candidates.length > 1)
            run_tot + apply_countChange(run_tot, coins.tail, candidates.tail)
          else
            run_tot
        else
          run_tot + apply_countChange(run_tot, coins, generate_children(candidates.head, coins)) + apply_countChange(run_tot, coins.tail, candidates.tail)
      }

      apply_countChange(0, coins, List(money))
  }
  }
