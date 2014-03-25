import org.scalatest.FunSuite
import scala.io.Source

class IntervalTreeTests extends FunSuite {

	def treeFromFile(filename: String): IntervalTree[Int] =  {
		val xt = new IntervalTree[Int]
		var n = 0
		for (line <- Source.fromFile(filename).getLines) {
			val token = line.split(",")
			val v = new Interval(token(0).toInt, token(1).toInt)
			xt.put(v, n)
			n = n + 1
		}
		xt
	}

	test("putting nodes in tree1") {
		val xt = treeFromFile("src/test/resources/tree1.txt")
		val answer = "(4, 8, 8):2 (5, 8, 18):1 (7, 10, 10):4 (15, 18, 18):3 " ++
                     "(17, 19, 24):0 (21, 24, 24):5 "
		assert(answer == xt.toString)
		xt.put(new Interval(16, 22), 6)
		val answer2 = "(4, 8, 8):2 (5, 8, 22):1 (7, 10, 10):4 (15, 18, 22):3 " ++
                     "(16, 22, 22):6 (17, 19, 24):0 (21, 24, 24):5 "
        assert(answer2 == xt.toString)
	}

}
