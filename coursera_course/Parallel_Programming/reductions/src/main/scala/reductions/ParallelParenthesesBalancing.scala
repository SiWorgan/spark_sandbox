package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balance_track(chars: Array[Char], stack: Int): Boolean = {
      if (stack < 0) false
      else if (chars.isEmpty) stack==0
      else if (chars.head == '(') balance_track(chars.tail, stack+1)
      else if (chars.head == ')') balance_track(chars.tail, stack-1)
      else balance_track(chars.tail, stack)
    }
    balance_track(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): Int /*: ???*/ = {
      if (arg1 < 0) arg1
      else if (idx == until) arg1
      else if (chars(idx) == '(') traverse(idx+1, until, arg1+1, arg2)
      else if (chars(idx) == ')') traverse(idx+1, until, arg1-1, arg2)
      else traverse(idx+1, until, arg1, arg2)
    }

    def reduce(from: Int, until: Int): Int /*: ???*/ = {
      if (until-from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid: Int = from+((until-from)/2)
        val (lft, rght) = parallel(reduce(from, mid), reduce(mid, until))
        if (lft<0) lft
        else lft+rght
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
