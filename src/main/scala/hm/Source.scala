package hm

import scala.util.Random

/**
  * @author Alexandru Stana, alexandru.stana@busymachines.com
  * @since 01/06/2018
  */
object Source extends App {
  import hm.alg.backtracking._
  import hm.alg.neighbourhood._
  import hm.alg.annealing._
  import hm.alg.genetic._

  val DS8   = DS.build(8)
  val DS10  = DS.build(10)
  val DS50  = DS.build(50)
  val DS100 = DS.build(100)

  val DSList = List(DS8, DS10, DS50, DS100)

  private def ex4 {
    DSList.take(2).foreach(ds => println(s"$ds: ${timer(ds.backtrack, alg = "B")}"))
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

  }

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

  private def timer[A](thunk: => A, sep: String = "| ", alg: String) = {
    val start  = System.currentTimeMillis()
    val result = thunk
    val end    = System.currentTimeMillis()
    print(s"$alg duration: ${end - start} milliseconds $sep")
    result
  }
}

case class DS(G: Int, N: Int, g: List[Int], v: List[Int]) {

  override def toString: String = s"DS-$N($G)"
}

object DS {

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
