package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("lineOfSight should correctly handle an array of size 1") {
    val output = new Array[Float](1)
    lineOfSight(Array[Float](0f), output)
    assert(output.toList == List(0f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("parlineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, 1)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("parlineOfSight should correctly handle an array of size 6") {
    val output = new Array[Float](6)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f, 1f, 2f), output, 3)
    assert(output.toList == List(0f, 1f, 4f, 4f, 4f, 4f))
  }

  test("parlineOfSight should correctly handle an array of size 1") {
    val output = new Array[Float](1)
    parLineOfSight(Array[Float](0f), output, 1)
    assert(output.toList == List(0f))
  }

  test("upsweep") {
    upsweep(Array[Float](0f, 2f, 2f, 3f, 6f, 5f, 18f, 14f, 24f, 18f), 1, 10, 4) == Node(Leaf(1,5,2.0f), Node(Leaf(5,7,3.0f) ,Leaf(7,10,3.0f)))
  }

  test("downsweep") {
    val output = new Array[Float](10)
    parLineOfSight(Array[Float](0f, 2f, 2f, 3f, 12f, 20f, 18f, 1f, 1f, 28f), output, 2)
    assert(output.toList == List(0.0f, 2.0f, 2.0f, 2.0f, 3.0f, 4.0f, 4.0f, 4.0f, 4.0f, 4.0f))
  }

}

