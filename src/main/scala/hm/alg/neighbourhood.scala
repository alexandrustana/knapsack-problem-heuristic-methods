package hm.alg

import hm.DS

import scala.annotation.tailrec

/**
  * @author Alexandru Stana, alexandru.stana@busymachines.com
  * @since 20/06/2018
  */
object neighbourhood {
  implicit class NS(ds: DS) {

    @tailrec
    final def neighbourhood(res: List[Int] = randomSolution(), iterations: Int = 50): Int =
      if (iterations == 0) res.map(ds.v(_)).sum
      else {
        val neighbours = neighbourhoodList(res).maxBy(l => l.map(ds.v(_)).sum)
        if (res.map(ds.v(_)).sum > neighbours.map(ds.v(_)).sum) res.map(ds.v(_)).sum
        else neighbourhood(neighbours,                                             iterations - 1)
      }

    private def neighbourhoodList(init: List[Int]) =
      init.map(
        i => nextStep(init.sum - init.filter(_ == i).head, init.filterNot(_ == i)).maxBy(l => l.map(ds.v(_)).sum)
      )

    @tailrec
    final def randomSolution(r: Int = 0, except: List[Int] = List.empty): List[Int] =
      if (ds.G == 0) except
      else {
        val i = ds.g.zipWithIndex.sortWith { case ((e1, _), (e2, _)) => e1 < e2 }.map { case (_, j) => j }
          .diff(except)
          .head
        if (except.contains(i)) randomSolution(r, except)
        else if (ds.G - ds.g(i) < 0) except
        else DS(ds.G - ds.g(i), ds.N, ds.g, ds.v).randomSolution(r + ds.v(i), i :: except)
      }

    private def nextStep(r: Int = 0, except: List[Int] = List.empty) =
      for {
        i <- (0 until ds.N).toList.diff(except)
        if ds.G - except.map(ds.g(_)).sum - ds.g(i) >= 0
      } yield i :: except
  }
}
