package com.cupenya
package intervaltree

import org.scalatest.FunSuite
import spire.implicits._
import spire.math.Interval

class IntervalTests extends FunSuite {
  val contains = Interval.openUpper(5D, 30D)
  val inside = Interval.openUpper(12D, 18D)
  val left = Interval.openUpper(5D, 15D)
  val none1 = Interval.openUpper(5D, 9D)
  val none2 = Interval.openUpper(21D, 300D)
  val right = Interval.openUpper(15D, 30D)
  val v = Interval.openUpper(10D, 20D)
  val (v2, v3) = (v, v)


  test("equals is reflexive (v == v)") {
    assert(v === v)
  }

  test("equals is symmetric (v2 == v and v == v2") {
    assert(v2 === v && v === v2)
  }
  test("equals is transitive (v == v2, v2 == v3 => v == v3") {
    assert(v == v2)
    assert(v2 == v3)
    assert(v == v3)
  }

  test("v intersects right") {
    assert(v intersects right)
    assert(right intersects v)
  }

  test("v intersects left") {
    assert(v intersects left)
    assert(left intersects v)
  }

  test("v intersects inside") {
    assert(v intersects inside)
    assert(inside intersects v)
  }

  test("v intersects contains") {
    assert(v intersects contains)
    assert(contains intersects v)
  }

  test("v intersects v2") {
    assert(v intersects v2)
    assert(v2 intersects v)
  }

  test("v intersects v3") {
    assert(v intersects v3)
    assert(v3 intersects v)
  }

  test("v does not intersect none1") {
    assert(!(v intersects none1))
    assert(!(none1 intersects v))
  }

  test("v does not intersect none2") {
    assert(!(v intersects none2))
    assert(!(none2 intersects v))
  }

  /*    test("v < right") {
        assert(v < right)
      }
      test("right > v") {
        assert(right > v)
      }
      test("v > left") {
        assert(v > left)
      }
      test("left < v") {
        assert(left < v)
      }

      test("v <= right") {
        assert(v <= right)
      }
      test("right >= v") {
        assert(right >= v)
      }
      test("v <= v") {
        assert(v >= v)
      }*/
}























