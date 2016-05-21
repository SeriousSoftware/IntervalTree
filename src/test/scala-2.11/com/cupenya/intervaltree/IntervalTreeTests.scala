package com.cupenya
package intervaltree

import org.scalatest.{FunSuite, Matchers}
import spire.implicits._
import spire.math.Interval

import scala.io.Source

class IntervalTreeTests extends FunSuite with Matchers {
  val pristine = List((co(4, 8), 3, 8), (co(5, 8), 2, 18), (co(7, 10), 5, 10),
    (co(15, 18), 4, 18), (co(17, 19), 1, 24), (co(21, 24), 6, 24))

  def treeFromFile(filename: String): IntervalTree[Int, Int] = {
    val xt = new IntervalTree[Int, Int]
    for (line <- Source.fromFile(filename).getLines) {
      val token = line.split(",")
      xt +=(co(token(0).trim.toInt, token(1).trim.toInt), token(2).trim.toInt)
    }
    xt
  }

  def co(n1: Int, n2: Int) = Interval.openUpper(n1, n2)

  test("+= successfully adds Intervals into a tree") {
    val xt = treeFromFile("src/test/resources/tree1.txt")
    val newNode = (co(16, 22), 7)
    xt.allNodes.map(_.getDebugInfo) should contain theSameElementsAs pristine

    xt += newNode

    val answer: List[(Interval[Int], Int)] = newNode :: pristine.map(m => (m._1, m._2))
    xt.allNodes.map(m => (m.getDebugInfo._1, m.getDebugInfo._2)) should contain theSameElementsAs answer
  }

  test("size of tree1 is 6") {
    val xt = treeFromFile("src/test/resources/tree1.txt")
    xt should have size 6
    xt +=(Interval(22, 23), 22)
    xt should have size 7
    xt +=(Interval(2000, 4000), 23)
    xt should have size 8
  }


  test("(5, 6) intersects intervals (5, 8) and (4, 8) in tree1") {
    val xt = treeFromFile("src/test/resources/tree1.txt")
    val xs: List[(Interval[Int], Int)] = xt.intersections(Interval(5, 6))
    val ms = List((co(4, 8), 3), (co(5, 8), 2))
    xs should contain theSameElementsAs ms
  }

  test("(1, 10) intersects values 2, 3 and 5 in tree1") {
    /*
    012345678901234567890123456789
    1|        |      [-)
    2|   [--) |
    3|  [---) |
    4|        |    [--)
    5|     [--)
    6|        |          [--)
     [        )
    */
    val xt = treeFromFile("src/test/resources/tree1.txt")
    val xs = xt.intersections(co(1, 10)).map(_._2)
    xs should contain theSameElementsAs List(2, 3, 5)
  }

  test("[4, 22) intersects all of the values in tree1") {
    val xt = treeFromFile("src/test/resources/tree1.txt")
    val xs = xt.intersections(co(4, 22)).map(_._2)
    xs should contain theSameElementsAs (1 to xt.size)
  }

  test("[10,15) does not intersect any intervals in tree1") {
    val xt = treeFromFile("src/test/resources/tree1.txt")
    val xs = xt.intersections(co(10, 15))
    xs shouldBe empty
  }

}
