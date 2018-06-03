package hm.alg

import hm.{DS, alg}

import scala.annotation.tailrec
import scala.util.Random

/**
  * @author Alexandru Stana, alexandru.stana@busymachines.com
  * @since 03/06/2018
  */
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
            annealing(
              initialTemperature,
              lengthTemperature,
              coolingRatio,
              result = temp,
              init   = temp,
              sample = sample + 1
            )
          }
          else {
            annealing(
              initialTemperature,
              lengthTemperature,
              coolingRatio,
              result = result,
              init   = temp,
              sample = sample + 1
            )
          }
        }
        else {
          if (Random.nextDouble() < nextState(initialTemperature, variation)) {
            annealing(
              initialTemperature,
              lengthTemperature,
              coolingRatio,
              result = result,
              init   = temp,
              sample = sample + 1
            )
          }
          else {
            annealing(
              initialTemperature,
              lengthTemperature,
              coolingRatio,
              result = result,
              init   = init,
              sample = sample + 1
            )
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

  implicit class GA(ds: DS) {
    import hm.alg.internal.KnapsackProblem

    import scala.collection.JavaConverters._

    def genetic(
                 populationSize: Int,
                 iterations:     Int,
                 crossoverRate:  Double,
                 mutationRate:   Double,
                 cloningRate:    Double
               ): Int = {
      new KnapsackProblem(
        ds.N,
        ds.G,
        ds.v.map(_.toDouble: java.lang.Double).asJava,
        ds.g.map(_.toDouble: java.lang.Double).asJava,
        populationSize,
        iterations,
        crossoverRate,
        mutationRate,
        cloningRate
      ).getResult.intValue()
    }
  }

}
