package com.cupenya.intervaltree

import java.time._
import java.time.temporal.ChronoUnit._

import org.scalatest.FunSuite
import spire.algebra.Order
import spire.math.Interval

// import scala.math.Ordering.Implicits._

class IntervalDateTests extends FunSuite {
  implicit object InstantOrdering extends Order[Instant] {
    def compare(lhs: Instant, rhs: Instant) = lhs compareTo rhs
  }

  val datum = Instant.now()
  val contains = Interval.openUpper(datum, datum.plus(25, DAYS))
  val inside = Interval.openUpper(datum.plus(7, DAYS), datum.plus(13, DAYS))
  val left = Interval.openUpper(datum, datum.plus(10, DAYS))
  val none1 = Interval.openUpper(datum, datum.plus(4, DAYS))
  val none2 = Interval.openUpper(datum.plus(16, DAYS), datum.plus(25, DAYS))
  val right = Interval.openUpper(datum.plus(10, DAYS), datum.plus(25, DAYS))
  val v = Interval.openUpper(datum.plus(5, DAYS), datum.plus(15, DAYS))
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