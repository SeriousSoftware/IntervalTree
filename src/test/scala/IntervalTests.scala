import org.scalatest.FunSuite

class IntervalTests extends FunSuite {
	val v        = new Interval(10, 20)
	val right    = new Interval(15, 30)
	val left     = new Interval(5, 15)
	val inside   = new Interval(12, 18)
	val contains = new Interval(5, 30)
	val v2       = new Interval(10, 20)
	val v3       = new Interval(10, 20)
	val none1    = new Interval(5, 9)
	val none2    = new Interval(21, 300)

	test("v == v")   { assert(v == v) }
	test("v2 == v")  { assert(v2 == v) }
	test("v == v2")  { assert(v == v2) }
	test("v2 == v3") { assert(v2 == v3) }
	test("v == v3")  { assert(v == v3) }

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























