package com.cupenya
package intervaltree

// TODO Use interval of spire lib


// import spire.implicits.{eqOps => _}
import spire.implicits._
import spire.math._

/**
  * Augmented tree
  *
  * @tparam T Numeric type of endpoints
  * @tparam P Payload type
  */
class IntervalTree[T: Numeric, P] {
  private var root: Option[Node] = None

  @inline protected[this] final def getBegin(v: Interval[T]) = v match {
    case Bounded(lower, _, _) => lower
    case _ => ???
  }

  def +=(p: (Interval[T], P)) {

    def put(x: Option[Node], v: Interval[T], value: P): Option[Node] =
      if (x.isEmpty) {
        Option(new Node(v, None, None,
          v match {
            case Bounded(_, upper, _) => upper
            case _ => ???
          },
          value))
      } else {
        if (getBegin(v) < getBegin(x.get.v)) {
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
        if (x.get.left.isDefined && x.get.left.get.max >= getBegin(v))
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

  class Node(val v: Interval[T],
             var left: Option[Node],
             var right: Option[Node],
             var max: T,
             val payLoad: P) {
    def getDebugInfo: (Interval[T], P, T) = (v, payLoad, max)

    override def toString = s"$v $payLoad $max"
  }

}
