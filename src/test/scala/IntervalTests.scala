import org.scalatest.FunSuite

class IntervalTests extends FunSuite {
	val v        = new Interval[Int](10, 20, 1)
	val right    = new Interval[Int](15, 30, 2)
	val left     = new Interval[Int](5, 15, 3)
	val inside   = new Interval[Int](12, 18, 4)
	val contains = new Interval[Int](5, 30, 5)
	val v2       = new Interval[Int](10, 20, 6)
	val v3       = new Interval[Int](10, 20, 7)
	val none1    = new Interval[Int](5, 9, 8)
	val none2    = new Interval[Int](21, 300, 9)

	test("equals is reflexive (v == v)")   { assert(v == v) }
	test("equals is symmetric (v2 == v and v == v2")  { 
		assert(v2 == v) 
	  assert(v == v2)
	}
	test("equals is transitive (v == v2, v2 == v3 => v == v3")  { 
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

	test("v < right") { assert(v < right) }
	test("right > v") { assert(right > v) }
	test("v > left")  { assert(v > left) }
	test("left < v")  { assert(left < v) }

	test("v <= right") { assert(v <= right) }
	test("right >= v") { assert(right >= v) }
	test("v <= v")     { assert(v >= v) }



}























