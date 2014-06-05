import org.scalatest.FunSuite
import scala.io.Source

class IntervalTreeTests extends FunSuite {

	def treeFromFile(filename: String): IntervalTree[Int] =  {
		val xt = new IntervalTree[Int]
		for (line <- Source.fromFile(filename).getLines) {
			val token = line.split(",")
			val v = new Interval[Int](token(0).toInt, token(1).toInt, token(2).toInt)
			xt += v
		}
		xt
	}

	test("+= successfully adds Intervals into a tree") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
    val answer = "4 8 3 8\n5 8 2 18\n7 10 5 10\n15 18 4 18\n17 19 1 24\n21 24 6 24\n"
		assert(answer == xt.toString)
		xt += new Interval(16, 22, 7)
  	val answer2 = "4 8 3 8\n5 8 2 22\n7 10 5 10\n15 18 4 22\n" +
  	              "16 22 7 22\n17 19 1 24\n21 24 6 24\n"
    assert(answer2 == xt.toString)
	}

	test("size of tree1 is 6") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
		assert(xt.size === 6)
		xt += new Interval(22, 22, 22)
		xt += new Interval(2000, 4000, 23)
		assert(xt.size === 8)
	}

	test("(5, 5) intersects intervals (5, 8, 2) and (4, 8, 3) in tree1") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
		val xs = xt.intersects(new Interval(5, 5, 1))
		val ms = List(new Interval(5, 8, 2), new Interval(4, 8, 3))
		assert(xs == ms)
	}

	test("(1, 10) intersects values 2, 3 and 5 in tree1") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
		val xs = xt.intersects(new Interval(1, 10, 1))
		val ms = List(new Interval(7, 10, 5), new Interval(5, 8, 2), 
		              new Interval(4, 8, 3))
		              
		assert(xs === ms)
	}

	test("(3, 25) intersects all of the values in tree1") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
		val xs = xt.intersects(new Interval(3, 25, 1))
		val ms = List(new Interval(21, 24, 6), 
		              new Interval(7, 10, 5),
		              new Interval(15, 18, 4),
		              new Interval(4, 8, 3),
		              new Interval(5, 8, 2),
		              new Interval(17, 19, 1))
		assert(xs === ms)
		// assert(xs === List(6, 5, 4, 3, 2, 1))
	}

	test("(14,14) does not intersect any intervals in tree1") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
		val xs = xt.intersects(new Interval(14, 14, 1))
		assert(xs === Nil)
	}
}
