package com.cupenya
package intervaltree

import org.scalatest.FunSuite
import spire.implicits._
import spire.math.{Bounded, Interval}

class IntervalTest extends FunSuite {
  val e = Interval.empty[Double]
  val all = Interval.all[Double]
  val a = cc(0.0, 4.0)
  val b = cc(-8.0, 2.0)
  val c = oc(0.0, 1.0)

  def cc(n1: Double, n2: Double) = Interval.closed(n1, n2)

  test("endpoint values can be retrieved") {
    assert((b match {
      case Bounded(lower, upper, flags: Int) => (lower, upper)
      case _ =>
    }) ===(-8.0, 2.0))
  }
  test("[2, inf] is a superset of empty") {
    assert(Interval.atOrAbove(2).isSupersetOf(Interval.empty[Int]))
  }
  test("empty is empty") {
    assert(e.isEmpty)
  }
  test("point is point") {
    assert(Interval.point(2).isPoint)
  }
  test("[2,2] is point") {
    assert(Interval.closed(2, 2).isPoint)
  }
  test("[3,2] is empty") {
    assert(Interval.closed(3, 2).isEmpty)
  }
  test("empty interval is not above -1") {
    assert(!Interval.empty[Int].hasAbove(-1))
  }
  test("empty interval is not below 1") {
    assert(!Interval.empty[Int].hasBelow(1))
  }
  test("[2] has above 0") {
    assert(Interval.point(2).hasAbove(0))
  }
  test("[-2] has below 0") {
    assert(Interval.point(-2).hasBelow(0))
  }
  test("[0, 1] has at or above 1") {
    assert(Interval.closed(0, 1).hasAtOrAbove(1))
  }
  test("[1, 2] has at or above 1") {
    assert(Interval.closed(1, 2).hasAtOrAbove(1))
  }
  test("[1, 2] has above 1") {
    assert(Interval.closed(1, 2).hasAtOrAbove(1))
  }
  test("(1, 2] has above 1") {
    assert(Interval.openLower(1, 2).hasAtOrAbove(1))
  }

  test("Interval.point(2).toString == [2]") {
    assert(Interval.point(2).toString == "[2]")
  }
  test("Interval.empty.toString == (Ø)") {
    assert(Interval.empty[Int].toString == "(Ø)")
  }

  def co(n1: Double, n2: Double) = Interval.openUpper(n1, n2)

  test("a.contains(0.0) is true") {
    assert(a.contains(0.0) === true)
  }
  test("a.crosses(0.0) is false") {
    assert(a.crosses(0.0) === false)
  }
  test("a.contains(3.334) is true") {
    assert(a.contains(3.334) === true)
  }
  test("a.contains(8.334) is false") {
    assert(a.contains(8.334) === false)
  }

  def oc(n1: Double, n2: Double) = Interval.openLower(n1, n2)

  test("b.contains(0.0) is true") {
    assert(b.contains(0.0) === true)
  }
  test("b.crosses(0.0) is true") {
    assert(b.crosses(0.0) === true)
  }

  def oo(n1: Double, n2: Double) = Interval.open(n1, n2)

  test("c.contains(0.0) is false") {
    assert(!c.contains(0.0))
  }
  test("c.crosses(0.0) is false") {
    assert(!c.crosses(0.0))
  }

  test("[3, 6] -- [3, 6] = nil") {
    assert(cc(3D, 6D) -- cc(3D, 6D) == Nil)
  }
  test("[3, 6] -- empty = [3, 6]") {
    assert(cc(3D, 6D) -- e == List(cc(3D, 6D)))
  }
  test("[3, 6] -- all = nil") {
    assert(cc(3D, 6D) -- all == Nil)
  }
  test("[3, 6] -- [4, 6] = [3, 4)") {
    assert(cc(3D, 6D) -- cc(4D, 6D) == List(co(3D, 4D)))
  }
  test("[3, 6] -- [4, 5] = [3, 4), (5, 6]") {
    assert(cc(3D, 6D) -- cc(4D, 5D) == List(co(3D, 4D), oc(5D, 6D)))
  }
}