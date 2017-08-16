package timeusage

import org.apache.spark.sql.{ColumnName, DataFrame, Row, Column}
import org.apache.spark.sql.types.{
  DoubleType,
  StringType,
  StructField,
  StructType
}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.apache.spark.{SparkConf, SparkContext}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {
  trait TestTimeUsage {
    val tu = TimeUsage
    val (columns, initDf) = tu.read("/timeusage/small_atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = tu.classifiedColumns(columns)
    val summaryDf = tu.timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
  }


  test("dfSchema") {
    val schema = timeusage.TimeUsage.dfSchema(List("A","B"))
    assert(schema.head.dataType == StringType)
    assert(schema.tail.head.dataType == DoubleType)
  }

  test("row") {
    val row: Row = timeusage.TimeUsage.row(List("A","0.4","0.2"))
    assert(row(0) == "A")
    assert(row(1) == 0.4d)
  }

  test("classifiedColumns") {
    val colTuple: (List[Column], List[Column], List[Column]) = timeusage.TimeUsage.classifiedColumns(List("t0101", "t18051", "t1888"))
    colTuple match {
      case (primaryCol: List[Column], workingCol: List[Column], leisureCol: List[Column]) =>
      {
        assert(primaryCol.contains(new Column("t0101")))
        assert(workingCol.contains(new Column("t18051")))
        assert(leisureCol.contains(new Column("t1888")))
      }
    }
  }

  test("timeUsageSummary") {
    new TestTimeUsage {
       assert(summaryDf.columns.length == 6)
       assert(summaryDf.first().getAs[String]("working") == "working")
       assert(summaryDf.first().getAs[String]("sex") == "male")
       assert(summaryDf.first().getAs[String]("age") == "elder")
    }
  }

  test("threeApproaches") {
    new TestTimeUsage {

      assert(tu.timeUsageGrouped(summaryDf).first().getAs[Double]("work") == tu.timeUsageGroupedSql(summaryDf).first().getAs[Double]("work"))
      assert(tu.timeUsageGrouped(summaryDf).first().getAs[Double]("work") == tu.timeUsageGroupedTyped(tu.timeUsageSummaryTyped(summaryDf)).first().work)
      assert(tu.timeUsageGroupedSql(summaryDf).first().getAs[Double]("work") == tu.timeUsageGroupedTyped(tu.timeUsageSummaryTyped(summaryDf)).first().work)
    }
  }

}
