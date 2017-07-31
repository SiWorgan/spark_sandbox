package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(this.empty),
    for {
      v <- arbitrary[A]
      h <- oneOf(const(this.empty), genHeap)
    } yield insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("two_elements") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    if (a < b) findMin(h) == a
    else findMin(h) == b
  }

  property("del_min") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("get_ordered") = forAll { (h: H) =>
    def listConst(h: H, lst: List[A]): Boolean = {
      if (isEmpty(h)) lst == lst.sorted
      else listConst(deleteMin(h), lst ::: List[A](findMin(h)))
    }
    listConst(h, List[A]())
  }

  property("min_meld") = forAll { (h1: H, h2: H) =>
    val a1 = if (isEmpty(h1)) 0 else findMin(h1)
    val a2 = if (isEmpty(h2)) 0 else findMin(h2)
    if (a1 < a2) findMin(insert(0, meld(h1, h2))) == a1
    else findMin(insert(0, meld(h1, h2))) == a2
  }

  property("empty_meld") = forAll { (h: H) =>
    h == meld(h, empty)
  }


}
