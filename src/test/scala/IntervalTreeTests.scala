import org.scalatest.FunSuite
import scala.io.Source

class IntervalTreeTests extends FunSuite {

	def treeFromFile(filename: String): IntervalTree[Int] =  {
		val xt = new IntervalTree[Int]
		for (line <- Source.fromFile(filename).getLines) {
			val token = line.split(",")
			xt.put(token(0).toInt, token(1).toInt, token(2).toInt)
		}
		xt
	}

	test("Successfully put tree1 nodes into tree") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
		val answer = "(4, 8, 8):3 (5, 8, 18):2 (7, 10, 10):5 (15, 18, 18):4 " ++
                     "(17, 19, 24):1 (21, 24, 24):6 "
		assert(answer == xt.toString)
		xt.put(16, 22, 7)
		val answer2 = "(4, 8, 8):3 (5, 8, 22):2 (7, 10, 10):5 (15, 18, 22):4 " ++
                     "(16, 22, 22):7 (17, 19, 24):1 (21, 24, 24):6 "
        assert(answer2 == xt.toString)
	}

	test("(5, 5) intersects values 2 and 3 tree1") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
		val xs = xt.intersects(5, 5)
		assert(xs == List(3, 2))
	}

	test("(1, 10) intersects values 2, 3 and 5 in tree1") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
		val xs = xt.intersects(1, 10)
		assert(xs === List(5, 3, 2))
	}

	test("(3, 25) intersects all of the values in tree1") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
		val xs = xt.intersects(3, 25)
		assert(xs === List(6, 5, 4, 3, 2, 1))
	}

	test("(14,14) does not intersect any intervals in tree1") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
		val xs = xt.intersects(14, 14)
		assert(xs === Nil)
	}
}
