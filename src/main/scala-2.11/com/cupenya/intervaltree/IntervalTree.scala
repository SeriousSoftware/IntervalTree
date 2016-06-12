package com.cupenya
package intervaltree

// TODO Use interval of spire lib

import spire.algebra.Order
import spire.implicits.{eqOps => _}
import spire.implicits._
import spire.math._
import spire.math.interval.{Bound, Closed, Open, ValueBound}

import scala.math.Ordering.Implicits._

/**
  * Augmented tree
  *
  * @tparam T Numeric type of endpoints
  * @tparam P Payload type
  */
class IntervalTree[T: Order, P] {
  private var root: Option[Node] = None

  sealed trait Nodes  // TODO Use instead of Option a NilNode class
  case class NilNode() extends Nodes

  class Node(val v: Interval[T],
             var left: Option[Node],
             var right: Option[Node],
             var max: Bound[T],
             val payLoad: P) extends Nodes {
    def getDebugInfo: (Interval[T], P, T) = (v, payLoad,
      max match {
        case Closed(a) => a
        case Open(a) => a
        case _ => ???
      })

    override def toString = s"$v $payLoad ${getDebugInfo._3}"
  }

  implicit object BoundOrdering extends Order[Bound[T]] {
    def compare(lhs: Bound[T], rhs: Bound[T]) =
      (lhs, rhs) match {
        case (ValueBound(lv), ValueBound(rv)) => lv compare  rv
      }
  }

  //@inline protected[this] final def getBegin(v: Interval[T]): Bound[T] = v.lowerBound

  def +=(p: (Interval[T], P)) {

    def put(x: Option[Node], v: Interval[T], value: P): Option[Node] =
      if (x.isEmpty) {
        Option(new Node(v, None, None,
          v.upperBound,
          value))
      } else {
        if (v.lowerBound < x.get.v.lowerBound) {
          x.get.left = put(x.get.left, v, value)
          x.get.max = max(x.get.left.get.max, x.get.max)
        } else {
          x.get.right = put(x.get.right, v, value)
          x.get.max = max(x.get.right.get.max, x.get.max)
        }
        x
      }

    root = put(root, p._1, p._2)
  }

  def -=(v: Interval[T]) = ???

  /**
    * Returns a List of (Interval[T], value[P]) pairs for each Interval
    * that intersects the interval v.
    */
  def intersections(v: Interval[T]): List[(Interval[T], P)] = {


    /**
      * Run time ~ R * lg(N) if there are N nodes in the tree and
      * the query interval intersects R nodes.  Also, since this is a
      * unbalanced BST, the worst case is ~ R * N.
      *
      * So try not to insert intervals in sorted order!
      */
    def intersections(x: Option[Node],
                      v: Interval[T],
                      vs: List[(Interval[T], P)]): List[(Interval[T], P)] = {
      if (x.isEmpty) vs
      else {
        val ys = if (v.intersects(x.get.v)) (x.get.v, x.get.payLoad) :: vs
        else vs
        if (x.get.left.isDefined && v.lowerBound < x.get.left.get.max )
          intersections(x.get.right, v, intersections(x.get.left, v, ys))
        else
          intersections(x.get.right, v, ys)
      }
    }

    intersections(root, v, List[(Interval[T], P)]())
  }

  def size = allNodes.size

  def allNodes: List[Node] = {
    def nodesToList(x: Option[Node]): List[Node] = {
      if (x.isEmpty) Nil
      else (nodesToList(x.get.left) :+ x.get) ++ nodesToList(x.get.right)
    }
    nodesToList(root)
  }

}