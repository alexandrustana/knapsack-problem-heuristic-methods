import scala.annotation.tailrec
import scala.util.Random

/**
  * @author Alexandru Stana, alexandru.stana@busymachines.com
  * @since 01/06/2018
  */
object Source extends App {
  import Algorithms._

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

  println("SA: " + DS8.annealing(1500, 500, 0.9))
  println("B: " + DS8.backtrack)

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

object Algorithms {
  import Math._

  implicit class B(ds: DS) {

    /*3*/
    def backtrack: Int = {

      if (ds.N == 0 || ds.G == 0) 0
      else if (ds.g(ds.N - 1) > ds.G) DS(ds.G, ds.N - 1, ds.g, ds.v).backtrack
      else
        max(
          ds.v(ds.N - 1) + DS(ds.G - ds.g(ds.N - 1), ds.N - 1, ds.g, ds.v).backtrack,
          DS(ds.G,                                   ds.N - 1, ds.g, ds.v).backtrack
        )
    }
  }

  implicit class NS(ds: DS) {

    @tailrec
    final def neighbourhood(res: List[Int] = randomSolution(), iterations: Int = 5): Int =
      if (iterations == 0) res.map(ds.v(_)).sum
      else {
        val neighbours = neighbourhoodList(res).maxBy(l => l.map(ds.v(_)).sum)
        if (res.map(ds.v(_)).sum > neighbours.map(ds.v(_)).sum) neighbourhood(res, iterations - 1)
        else neighbourhood(neighbours,                                             iterations - 1)
      }

    private[NS] def neighbourhoodList(init: List[Int]) =
      init.map(
        i => nextStep(init.sum - init.filter(_ == i).head, init.filterNot(_ == i)).maxBy(l => l.map(ds.v(_)).sum)
      )

    @tailrec
    private[NS] final def randomSolution(r: Int = 0, except: List[Int] = List.empty): List[Int] =
      if (ds.G == 0) except
      else {
        val i = if (except.length == ds.N - 1) (0 until ds.N).toList.diff(except).head else Random.nextInt(ds.N - 1)
        if (except.contains(i)) randomSolution(r, except)
        else if (ds.G - ds.g(i) < 0) except
        else DS(ds.G - ds.g(i), ds.N, ds.g, ds.v).randomSolution(r + ds.v(i), i :: except)
      }

    private[NS] def nextStep(r: Int = 0, except: List[Int] = List.empty) =
      for {
        i <- (0 until ds.N).toList.diff(except)
        if ds.G - except.map(ds.g(_)).sum - ds.g(i) >= 0
      } yield i :: except
  }

  implicit class SA(ds: DS) {

    @tailrec
    final def annealing(
      initialTemperature: Double,
      lengthTemperature:  Double,
      coolingRatio:       Double,
      init:               List[Byte] = initialSolution(ds.N),
      result:             List[Byte] = initialSolution(ds.N),
      sampleSize:         Int = 50,
      sample:             Int = 0
    ): Int = {
      if (initialTemperature <= lengthTemperature) calculate(result, ds.v)
      else if (sample == sampleSize)
        annealing(initialTemperature * coolingRatio, lengthTemperature, coolingRatio, init, result)
      else {
        val i         = Random.nextInt(ds.N)
        val temp      = init.take(i) ++ ({ if (init(i) == 0) 1 else 0 }.toByte :: init.drop(i + 1))
        val variation = calculate(temp, ds.v) - calculate(init, ds.v)
        if (variation > 0 && calculate(temp, ds.g) < ds.G) {
          if (calculate(temp, ds.v) > calculate(result, ds.v)) {
            annealing(initialTemperature, lengthTemperature, coolingRatio, result = temp, init = temp, sample = sample + 1)
          }
          else {
            annealing(initialTemperature, lengthTemperature, coolingRatio, result = result, init = temp, sample = sample + 1)
          }
        }
        else {
          if (Random.nextDouble() < nextState(initialTemperature, variation)) {
            annealing(initialTemperature, lengthTemperature, coolingRatio, result = result, init = temp, sample = sample + 1)
          }
          else {
            annealing(initialTemperature, lengthTemperature, coolingRatio, result = result, init = init, sample = sample + 1)
          }
        }
      }
    }

    private def nextState(t: Double, v: Double): Double = {
      Math.exp((v - v) / t)
    }

    private def calculate(s: List[Byte], l: List[Int]): Int =
      s.zipWithIndex.map { case (e, i) => if (e == 1) l(i) else 0 }.sum

    private def initialSolution(N: Int): List[Byte] = List.fill(N)(0)
  }
}
