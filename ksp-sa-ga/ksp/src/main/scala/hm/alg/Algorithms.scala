package hm.alg

import hm.DS

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
    final def neighbourhood(res: List[Int] = randomSolution(), iterations: Int = 50): Int =
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
    private[Algorithms] final def randomSolution(r: Int = 0, except: List[Int] = List.empty): List[Int] =
      if (ds.G == 0) except
      else {
        val i = ds.g.zipWithIndex.sortWith { case ((e1, _), (e2, _)) => e1 < e2 }.map { case (_, j) => j }
          .diff(except)
          .head
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
    private val random = ds.randomSolution()
    private val initial = List.fill(ds.N)(0).zipWithIndex.map{ case(_, i) => {if(random.contains(i)) 1 else 0}.toByte}

    @tailrec
    final def annealing(
      temperature:       Double,
      lengthTemperature: Double,
      coolingRatio:      Double,
      current:           List[Byte] = initial,
      result:            List[Byte] = initial,
      sampleSize:        Int = 50,
      sample:            Int = 0
    ): Int = {
      if (temperature <= lengthTemperature) calculate(result, ds.v)
      else if (sample == sampleSize)
        annealing(temperature * coolingRatio, lengthTemperature, coolingRatio, current, result)
      else {
        val i = Random.nextInt(ds.N)
        val temp = current.updated(i, {
          if (current(i) == 0) 1 else 0
        }.toByte)
        val variation = calculate(temp, ds.v) - calculate(current, ds.v)
        if (variation > 0 && calculate(temp, ds.g) < ds.G)
          annealing(
            temperature,
            lengthTemperature,
            coolingRatio,
            result  = if (calculate(temp, ds.v) > calculate(result, ds.v)) temp else result,
            current = temp,
            sample  = sample + 1
          )
        else {
          val x = Random.nextDouble()
          annealing(
            temperature,
            lengthTemperature,
            coolingRatio,
            result  = result,
            current = if (x < Math.exp(-variation / temperature)) temp else current,
            sample  = sample + 1
          )
        }
      }
    }

    private def calculate(s: List[Byte], l: List[Int]): Int =
      s.zipWithIndex.map { case (e, i) => if (e == 1) l(i) else 0 }.sum
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
