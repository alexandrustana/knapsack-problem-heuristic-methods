package hm

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

  private def ex4 {
    DSList.take(2).foreach(ds => println(s"$ds: ${timer(ds.backtrack, alg = "B")}"))
    /*Using the backtracking algorithm on small sets offers solutions in a decent amount of time
   * the problem is if the data sets become larger(e.g. 50, 100). The backtracking algorithm evaluates some
   * solutions multiples times this causing the call stack to increase and overflow if the data set is large enough.*/
  }

  private def ex5 {
    DSList.foreach(ds => println(s"$ds: ${ds.neighbourhood()} NS"))
    DSList.take(2).foreach(ds => println(s"$ds: ${ds.backtrack} B vs ${ds.neighbourhood()} NS"))
  }

  private def ex6 {
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

  private def ex8 {
    List((1500, 500, 0.9), (15000, 1500, 0.99), (150000, 15000, 0.999)).foreach(
      i => DSList.foreach(ds => println(s"$ds: ${timer(ds.annealing(i._1, i._2, i._3), alg = s"SA$i")}"))
    )
  }

  private def ex9 {
    List((1000, 50, 0.8, 0.15, 0.5), (50, 1000, 0.6, 0.3, 0.1), (500, 500, 0.7, 0.2, 0.3)).foreach(
      i => DSList.foreach(ds => println(s"$ds: ${timer(ds.genetic(i._1, i._2, i._3, i._4, i._5), alg = s"GA$i")}"))
    )
  }

  private def ex10 {
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

    DSList
      .take(3)
      .foreach(ds => println(s"$ds: ${timer(ds.backtrack, alg = "B")} vs ${timer(ds.neighbourhood(), alg = "NS")}"))
  }

  /*11*/
  /*The genetic algorithm shows a better precision when computing the results rather than the simulated annealing
   * algorithm. This better precision is due to the fact that the genetic algorithm generates a population of solutions
   * and the simulated annealing algorithm has only one solution. On large data sets the genetic algorithm performs
   * better than the simulated annealing algorithm*/

  private def ex12 {
    List(
      ((1500,   500,   0.9),   (1000, 50,   0.8, 0.15, 0.5)),
      ((15000,  1500,  0.99),  (50,   1000, 0.6, 0.3,  0.1)),
      ((150000, 15000, 0.999), (500,  500,  0.7, 0.2,  0.3))
    ).foreach(
      i =>
        DSList
          .foreach(
            ds =>
              println(
                s"$ds: ${timer(ds.annealing(i._1._1, i._1._2, i._1._3), alg = s"SA${i._1}")} vs " +
                  s"${timer(ds.genetic(i._2._1,      i._2._2, i._2._3, i._2._4, i._2._5), alg = s"GA${i._2}")} vs " +
                  s"${timer(ds.neighbourhood(), alg = "NS")}"
            )
        )
    )
  }

  private def ex13 {
    List(
      (1500,   500,   0.9),
      (1500,   500,   0.4),
      (15000,  1500,  0.99),
      (15000,  1500,  0.3),
      (150000, 15000, 0.5),
      (150000, 15000, 0.9)
    ).foreach(
      i =>
        DSList.foreach(
          ds =>
            println(
              s"$ds: ${timer(ds.annealing(i._1, i._2, i._3), alg = s"SA$i")}"
          )
      )
    )
  }
  /*The bigger the gap between the initial temperature and the temperature length is the more accurate the result is.
   * The accuracy the result is mostly affected by the cooling factor. A high cooling factor means that the temperatures
   * will drop down fast and solutions will be missed. On the other hand a low cooling factor will ensure a more granular
   * approach and will offer much more solutions.*/

  private def ex14 {
    List(
      (1000, 50,   0.8, 0.15, 0.5),
      (1000, 50,   0.2, 0.15, 0.5),
      (1000, 50,   0.8, 0.7,  0.5),
      (1000, 50,   0.8, 0.15, 0.2),
      (50,   1000, 0.8, 0.15, 0.5),
      (50,   1000, 0.2, 0.15, 0.5),
      (50,   1000, 0.8, 0.7,  0.5),
      (50,   1000, 0.8, 0.15, 0.2),
      (500,  500,  0.8, 0.15, 0.5),
      (500,  500,  0.2, 0.15, 0.5),
      (500,  500,  0.8, 0.7,  0.5),
      (500,  500,  0.8, 0.15, 0.2)
    ).foreach(
      i =>
        DSList.foreach(
          ds =>
            println(
              s"$ds: ${timer(ds.genetic(i._1, i._2, i._3, i._4, i._5), alg = s"GA$i")}"
          )
      )
    )
  }

  /**
    * A high number of generations ensures that the result is more accurate, however if the number is to large the time
    * needed to find a solution is much longer. Even tough a small number of generations yields a result much faster this
    * comes with the price that some good solutions will never be reached. If the mutation rate is high than there is
    * the risk that some good solutions are mutated and invalidated, if the mutation rate is to low than the algorithm
    * can get stuck. If the crossover rate is to high than the problem with the mutation high rate remains, some valid
    * genes which offer good solutions are combined and the result is not as good as the parent gene, but if the crossover
    * rate is to low than again the algorithm can get stuck. Having a low cloning rate can cause the algorithm to evolve
    * slowly because the new genes can have a lower score than the old genes, however having a high cloning rate will
    * cause the new population to be very much like the old one and thus it may get stuck.
    */

  /*15*/
  /*The advantage of using a Backtracking algorithm is that the algorithm ensures that the solution is always the best.
   * Although this best solution comes with the cost that the call stack explodes if the Data Set is not small, thus making
   * the algorithm feasible to use only for small data sets.
   * The Neighbourhood Search algorithm on the other hand can be used with much bigger data sets, because it doesn't
   * find the best solution exhaustively. The algorithm after a number of iterations quits and returns the best solution
   * found so far. The problem with this algorithm is that it can get stuck in a local solution.
   * The Simulated Annealing algorithm has the advantage that it doesn't get stuck in a local solution but because it only
   * uses a single "population" as input the chances for it to offer the best solution are smaller than the chances for
   * the Genetic Algorithm to offer the best solution.
   * The Genetic Algorithm or the Simulated Annealing algorithm although they don't offer all the time the best solution
   * they can still offer better solutions than the Neighbourhood Search algorithm(which can get stuck) or the Backtracking
   * algorithm which can easily explode the call stack.*/

  private def timer[A](thunk: => A, sep: String = "| ", alg: String) = {
    val start  = System.currentTimeMillis()
    val result = thunk
    val end    = System.currentTimeMillis()
    print(s"$alg duration: ${end - start} milliseconds $sep")
    result
  }
}

/*1*/
case class DS(G: Int, N: Int, g: List[Int], v: List[Int]) {

  override def toString: String = s"DS-$N($G)"
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
