package hm

import scala.annotation.tailrec
import scala.util.Random

/**
  * @author Alexandru Stana, alexandru.stana@busymachines.com
  * @since 01/06/2018
  */
object Source extends App {
  import alg.Algorithms._

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

  /*7*/
  /*Although backtracking algorithm offers the best solution for the problem at hand it is quite restrictive regarding
   * the size of the data set (i.e. the algorithm takes O(2^n) time to complete), thus making it impossible to use with
   * quite small data sets(e.g. 100 entries). On the other hand the neighborhood search algorithm does not have this
   * time constraint and also it doesn't overflow the call stack (i.e. for storing its neighbours it uses the heap stack)
   * but it also doesn't find all the time the best solution.*/

  def ex8 = {
    List((1500, 500, 0.9), (15000, 1500, 0.99), (150000, 15000, 0.999)).foreach(
      i => DSList.foreach(ds => println(s"$ds: ${timer(ds.annealing(i._1, i._2, i._3), alg = s"SA$i")}"))
    )
  }

  def ex9 = {
    /*9*/
    List((1000, 50, 0.8, 0.15, 0.5), (50, 1000, 0.6, 0.3, 0.1), (500, 500, 0.7, 0.2, 0.3)).foreach(
      i => DSList.foreach(ds => println(s"$ds: ${timer(ds.genetic(i._1, i._2, i._3, i._4, i._5), alg = s"GA$i")}"))
    )
  }

  def ex10 = {
    /*10*/
    List(
      ((1500,   500,   0.9),   (1000, 50,   0.8, 0.15, 0.5)),
      ((15000,  1500,  0.99),  (50,   1000, 0.6, 0.3,  0.1)),
      ((150000, 15000, 0.999), (500,  500,  0.7, 0.2,  0.3))
    ).foreach(
      i =>
        DSList.foreach(
          ds =>
            println(
              s"$ds: ${timer(ds.annealing(i._1._1, i._1._2, i._1._3), alg = s"SA${i._1}")} vs " +
                s"${timer(ds.genetic(i._2._1,      i._2._2, i._2._3, i._2._4, i._2._5), alg = s"GA${i._2}")}"
          )
      )
    )
  }

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
