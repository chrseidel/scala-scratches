import scala.concurrent.duration.NANOSECONDS

val start = System.nanoTime()
object A {
  case class Square(n: Int, square: Int)
  def squares(n: Int = 0, nSquared: Int = 0): Stream[Square] = Square(n, nSquared) #:: squares(n+1, nSquared + 2 * n + 1)

  val squaresWithNumOfRoots = squares(0, 0).takeWhile(_.square <= 1000000000)
    .foldLeft(scala.collection.immutable.TreeMap[Int, Int]())((tree, square) => {
      tree.insert(square.square, tree.get(square.n).map(_ + 1).getOrElse(1))
    })

  def solution(a: Int, b: Int): Int = {
    val subMap = squaresWithNumOfRoots.from(a).to(b)

    subMap match {
      case map if map.isEmpty => 0
      case map => map.maxBy(_._2)._2
    }
  }
}


A.solution(2, 9) == 1
A.solution(9025, 9025) == 1
A.solution(1396, 1396) == 0
A.solution(2, 1000000000) == 4

val latency = NANOSECONDS.toMillis(System.nanoTime()-start)/4
