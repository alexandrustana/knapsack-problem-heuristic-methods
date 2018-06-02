import scala.annotation.tailrec
import scala.util.Random

/**
  * @author Alexandru Stana, alexandru.stana@busymachines.com
  * @since 01/06/2018
  */
object Source extends App {
  val DS8   = DS.build(8)
  val DS10  = DS.build(10)
  val DS50  = DS.build(50)
  val DS100 = DS.build(100)

  val DSList = List(DS8, DS10, DS50, DS100)

  def ex4 = {
    /*4*/
    DSList.take(2).foreach(ds => println(s"$ds: ${timer(ds.backtrack, alg = "B")}"))
    /*Using the backtracking algorithm on small sets offers solutions in a decent amount of time
   * the problem is if the data sets become larger(e.g. 50, 100). The backtracking algorithm evaluates some
   * solutions multiples times this causing the call stack to increase and overflow if the data set is large enough.*/
  }

  def ex5 = {
    /*5*/
    DSList.foreach(ds => println(s"$ds: ${ds.neighbourhood()} NS"))
    DSList.take(2).foreach(ds => println(s"$ds: ${ds.backtrack} B vs ${ds.neighbourhood()} NS"))
  }

  def ex6 = {
    /*6*/
    DSList.take(2).foreach(ds => println(s"$ds: ${timer(ds.neighbourhood(),         alg = "NS")}"))
    DSList.reverse.take(2).foreach(ds => println(s"$ds: ${timer(ds.neighbourhood(), alg = "NS")}"))

    DSList
      .take(2)
      .foreach(ds => println(s"$ds: ${timer(ds.backtrack, alg = "B")} vs ${timer(ds.neighbourhood(), alg = "NS")}"))

    DSList.foreach(ds => println(s"$ds: ${timer(ds.neighbourhood(), alg = "NS")}"))

    /*The neighborhood search algorithm although it doesn't find the best solution(because it doesn't look at all the
   * possible solutions) it still offers optimal solutions despite the size of the data set, unlike the backtracking
   * algorithm which call stack can overflow with a relative small data set*/
  }
  ex6

  /*7*/
  /*Although backtracking algorithm offers the best solution for the problem at hand it is quite restrictive regarding
   * the size of the data set (i.e. the algorithm takes O(2^n) time to complete), thus making it impossible to use with
   * quite small data sets(e.g. 100 entries). On the other hand the neighborhood search algorithm does not have this
   * time constraint and also it doesn't overflow the call stack (i.e. for storing its neighbours it uses the heap stack)
   * but it also doesn't find all the time the best solution.*/

  def timer[A](thunk: => A, sep: String = "| ", alg: String) = {
    val start  = System.currentTimeMillis()
    val result = thunk
    val end    = System.currentTimeMillis()
    print(s"$alg duration: ${end - start} milliseconds $sep")
    result
  }
}

/*1*/
case class DS(G: Int, N: Int, g: List[Int], v: List[Int]) {

  override def toString: String = s"DS-$N"

  /*3*/
  def backtrack: Int = {
    import Math._

    if (N == 0 || G == 0) 0
    else if (g(N - 1) > G) DS(G, N - 1, g, v).backtrack
    else max(v(N - 1) + DS(G - g(N - 1), N - 1, g, v).backtrack, DS(G, N - 1, g, v).backtrack)
  }

  @tailrec
  final def neighbourhood(res: List[Int] = randomSolution(), iterations: Int = 5): Int =
    if (iterations == 0) res.map(v(_)).sum
    else {
      val neighbours = neighbourhoodList(res).maxBy(l => l.map(v(_)).sum)
      if (res.map(v(_)).sum > neighbours.map(v(_)).sum) neighbourhood(res, iterations - 1)
      else neighbourhood(neighbours,                                       iterations - 1)
    }

  private def neighbourhoodList(init: List[Int]) =
    init.map(i => nextStep(init.sum - init.filter(_ == i).head, init.filterNot(_ == i)).maxBy(l => l.map(v(_)).sum))

  @tailrec
  private def randomSolution(r: Int = 0, except: List[Int] = List.empty): List[Int] =
    if (G == 0) except
    else {
      val i = if (except.length == N - 1) (0 until N).toList.diff(except).head else Random.nextInt(N - 1)
      if (except.contains(i)) randomSolution(r, except)
      else if (G - g(i) < 0) except
      else DS(G - g(i), N, g, v).randomSolution(r + v(i), i :: except)
    }

  private def nextStep(r: Int = 0, except: List[Int] = List.empty) =
    for {
      i <- (0 until N).toList.diff(except)
      if G - except.map(g(_)).sum - g(i) >= 0
    } yield i :: except

}

object DS {

  /*2*/
  def build(N: Int, threshold: Int = 100): DS = {
    val G = Random.nextInt(threshold) + 1
    DS(
      G = G,
      N = N,
      g = (1 to N).map(_ => Random.nextInt(if (G / 2 == 0) 1 else G / 2) + 1).toList,
      v = (1 to N).map(_ => Random.nextInt(threshold) + 1).toList
    )
  }
}
