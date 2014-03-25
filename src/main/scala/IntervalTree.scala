class IntervalTree[T] {

	private var root: Node = null;

	class Node(var v:        Interval, 
		       var left:     Node,
		       var right:    Node,
		       var maxHi:    Int,
		       var value:    T
		      )

	def put(v: Interval, value: T) {
		root = put(root, v, value)
	}

	private def put(x: Node, v: Interval, value: T): Node = {
		if (x == null) {
			return new Node(v, null, null, v.hi, value)
		} else if (v < x.v) {
			x.left = put(x.left, v, value)
			if (x.left.maxHi > x.maxHi) x.maxHi = x.left.maxHi
			return x
		} else {
			x.right = put(x.right, v, value)
			if (x.right.maxHi > x.maxHi) x.maxHi = x.right.maxHi
			return x
		}
	}

	def get(lo: Int, hi: Int): T = ???

	def delete(lo: Int, hi: Int): IntervalTree[T] = ???

	def intersects(lo: Int, hi: Int): Iterable[T] = ???	
	
	override def toString = {
		def treeString(x: Node): String = {
			if (x == null) {
				""
			} else {
				treeString(x.left) ++
				s"(${x.v.lo}, ${x.v.hi}, ${x.maxHi}):${x.value} " ++
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