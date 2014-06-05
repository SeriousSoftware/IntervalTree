class IntervalTree[T] {

	private var root: Node = null;

	class Node(var v: Interval,
		         var left: Node,
		         var right: Node,
		         var max: Int,
		         var value: T)

	def put(lo: Int, hi: Int, value: T) {
		root = put(root, new Interval(lo, hi), value)
	}

	private def put(x: Node, v: Interval, value: T): Node = {
		if (x == null) {
			new Node(v, null, null, v.hi, value)
		} else if (v < x.v) {
			x.left = put(x.left, v, value)
			x.max = x.left.max max x.max
			x
		} else {
			x.right = put(x.right, v, value)
			x.max = x.right.max max x.max
			x
		}
	}

	def get(lo: Int, hi: Int): T = ???

	def delete(lo: Int, hi: Int): IntervalTree[T] = ???

	/**
	 * Get all of the values that intersect the interval (lo, hi).
	 */
	def intersects(lo: Int, hi: Int): Iterable[T] = 
		intersects(root, new Interval(lo, hi), List[T]())

	/**
	 * Run time ~ R * lg(N) if there are N nodes in the tree and 
	 * the query interval intersects R nodes.  Also, since this is a 
	 * unbalanced BST, the worst case is ~ R * N. 
	 *
	 * So try not to insert intervals in sorted order!
	 */
	private def intersects(x: Node, v: Interval, xs: List[T]): List[T] = {
		if (x == null) xs
		else {
			val ys = if (v intersects x.v) x.value :: xs else xs
			if (x.left != null && x.left.max >= v.lo) 
				intersects(x.right, v, intersects(x.left, v, ys))
			else
				intersects(x.right, v, ys)
		}
	}
	
	override def toString = {
		def treeString(x: Node): String = {
			if (x == null) {
				""
			} else {
				treeString(x.left) ++
				s"(${x.v.lo}, ${x.v.hi}, ${x.max}):${x.value} " ++
				treeString(x.right)
			}
		}

		treeString(root)
	}
}

class Interval(val lo: Int, val hi: Int) extends Ordered[Interval] {

	def intersects(that: Interval): Boolean = 
		this == that                             ||
		that.lo >= this.lo && that.lo <= this.hi ||
		this.lo >= that.lo && this.lo <= that.hi ||
		this.lo <= that.lo && this.hi >= that.hi ||
		this.lo >= that.lo && this.hi <= that.hi
	
	def canEqual(a: Any) = a.isInstanceOf[Interval]

	override def equals(that: Any): Boolean = 
		that match {
			case that: Interval => that.canEqual(this) &&
			                     that.lo == that.lo  &&
			                     this.hi == that.hi
			case _              => false
        }

	/**
	 * Compares inteverals by their lo values.
	 */
	def compare(that: Interval) = this.lo compare that.lo

}