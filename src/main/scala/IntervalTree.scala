class IntervalTree[T] {

	private var nnodes: Long = 0
	private var root: Node = null;

	class Node(var v: Interval[T],
		         var left: Node,
		         var right: Node,
		         var max: Int)
		         

	def +=(v: Interval[T]) {

		def put(x: Node, v: Interval[T]): Node = {
			if (x == null) {
				nnodes = nnodes + 1
				new Node(v, null, null, v.hi)
			} else if (v < x.v) {
				x.left = put(x.left, v)
				x.max = x.left.max max x.max
				x
			} else {
				x.right = put(x.right, v)
				x.max = x.right.max max x.max
				x
			}
		}

		root = put(root, v)
	}

	def -=(v: Interval[T]) = ???

	/**
	 * Returns a List of all of the intervals that intersect the interval v.
	 */
	def intersects(v: Interval[T]): Iterable[Interval[T]] = 
		intersects(root, v, List[Interval[T]]())

	/**
	 * Run time ~ R * lg(N) if there are N nodes in the tree and 
	 * the query interval intersects R nodes.  Also, since this is a 
	 * unbalanced BST, the worst case is ~ R * N. 
	 *
	 * So try not to insert intervals in sorted order!
	 */
	private def intersects(x: Node, 
	                       v: Interval[T], 
	                       vs: List[Interval[T]]): List[Interval[T]] = 
	{
		if (x == null) vs
		else {
			val ys = if (v intersects x.v) x.v :: vs else vs
			if (x.left != null && x.left.max >= v.lo) 
				intersects(x.right, v, intersects(x.left, v, ys))
			else
				intersects(x.right, v, ys)
		}
	}
	
	def size = nnodes

	/**
	 * Each node is written on a separate line where each line has the form
	 *
	 * lo hi value max
	 */
	override def toString = {
		def treeString(x: Node): String = {
			if (x == null) {
				""
			} else {
				treeString(x.left) ++
				s"${x.v} ${x.max}\n" ++
				treeString(x.right)
			}
		}

		treeString(root)
	}
}

class Interval[T](val lo: Int, val hi: Int, value: T) extends Ordered[Interval[T]] {
	require(lo <= hi, s"\n\t\tInterval: lo <= hi? lo == $lo  hi == $hi")

	def intersects(that: Interval[T]): Boolean = 
		this == that                             ||
		that.lo >= this.lo && that.lo <= this.hi ||
		this.lo >= that.lo && this.lo <= that.hi ||
		this.lo <= that.lo && this.hi >= that.hi ||
		this.lo >= that.lo && this.hi <= that.hi
	
	def canEqual(a: Any) = a.isInstanceOf[Interval[T]]

	/**
	 * Two inteverals are equal if their endpoints are equal.
	 * Values are not considered testing for equality; i.e., 
	 * intervals can have different values yet still be equal.
	 */
	override def equals(that: Any): Boolean = 
		that match {
			case that: Interval[T] => that.canEqual(this) &&
			                          that.lo == that.lo  &&
			                          this.hi == that.hi
			case _                 => false
    }

	/**
	 * Compares inteverals by their lo values.
	 */
	def compare(that: Interval[T]) = this.lo compare that.lo

	override def toString = s"$lo $hi $value"

}