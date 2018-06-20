package hm.alg

import hm.DS

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
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
        if (res.map(ds.v(_)).sum > neighbours.map(ds.v(_)).sum) res.map(ds.v(_)).sum
        else neighbourhood(neighbours,                                             iterations - 1)
      }

    private def neighbourhoodList(init: List[Int]) =
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

    private def nextStep(r: Int = 0, except: List[Int] = List.empty) =
      for {
        i <- (0 until ds.N).toList.diff(except)
        if ds.G - except.map(ds.g(_)).sum - ds.g(i) >= 0
      } yield i :: except
  }

  implicit class SA(ds: DS) {
    private val random = ds.randomSolution()
    private val initial =
      List.fill(ds.N)(0).zipWithIndex.map {
        case (_, i) => {
          if (random.contains(i)) 1 else 0
        }.toByte
      }

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

    private val bestSolutionsOfGeneration = ArrayBuffer.empty[List[Byte]]
    private val breedPopulation           = ArrayBuffer.empty[List[Byte]]
    private val meanFitnessOfGeneration   = ArrayBuffer.empty[Double]
    private val bestFitnessOfGeneration   = ArrayBuffer.empty[Double]
    private val random                    = new Random()

    def genetic(
      populationSize: Int,
      iterations:     Int,
      crossoverRate:  Double,
      mutationRate:   Double,
      cloningRate:    Double
    ): Int = {
      val population = ArrayBuffer(create(populationSize, ds.N).toArray: _*)

      var evaluated = evaluate(ds.N, ds.G, population.toList, ds.g, ds.v)

      bestSolutionsOfGeneration += population(bestSolution(evaluated._1))

      meanFitnessOfGeneration += meanFitness(evaluated._2, populationSize)

      bestFitnessOfGeneration += evaluateGene(ds.N, ds.G, population(bestSolution(evaluated._1)), ds.g, ds.v)

      (0 until iterations).foreach(_ => {
        (0 until (populationSize / 2))
          .foreach(
            _ => breed(populationSize, evaluated, crossoverRate, mutationRate, cloningRate, ds.N, population.toList)
          )

        population.clear()
        breedPopulation.foreach(e => population += e)
        breedPopulation.clear()

        evaluated = evaluate(ds.N, ds.G, population.toList, ds.g, ds.v)

        bestSolutionsOfGeneration += population(bestSolution(evaluated._1))

        meanFitnessOfGeneration += meanFitness(evaluated._2, populationSize)

        bestFitnessOfGeneration += evaluateGene(ds.N, ds.G, population(bestSolution(evaluated._1)), ds.g, ds.v)
      })

      result(iterations, ds.N, ds.v)
    }

    private def result(iterations: Int, items: Int, values: List[Int]) = {
      val fitness =
        bestFitnessOfGeneration.zipWithIndex.foldLeft((0.toDouble, 0))((max, e) => if (e._1 > max._1) e else max)
      bestSolutionsOfGeneration(fitness._2).zipWithIndex.filter(e => e._1 == 1).map(e => values(e._2)).sum
    }

    private def breed(
      size:          Int,
      fitness:       (List[Int], Int),
      probCrossover: Double,
      probMutation:  Double,
      probCloning:   Double,
      items:         Int,
      population:    List[List[Byte]]
    ) {
      crossover(
        population(selectGene(size, fitness)),
        population(selectGene(size, fitness)),
        probCrossover,
        items
      )
      mutate(probMutation, items)
      cloning(probCloning, population)
    }

    private def cloning(probCloning: Double, population: List[List[Byte]]) {
      val x = random.nextDouble()
      if (x <= probCloning) {
        breedPopulation += bestSolutionsOfGeneration(bestSolutionsOfGeneration.size - 1)
      }
      ()
    }

    private def crossover(
      gene1:         List[Byte],
      gene2:         List[Byte],
      probCrossover: Double,
      items:         Int,
    ) {
      val x = random.nextDouble()
      if (x <= probCrossover) {
        val crossPoint = random.nextInt(items) + 1
        val newGene1 = gene1.slice(0, crossPoint) ++
          gene2.slice(crossPoint, gene2.size)
        val newGene2 = gene2.slice(0, crossPoint) ++
          gene1.slice(crossPoint, gene1.size)

        breedPopulation += newGene1
        breedPopulation += newGene2
      }
      ()
    }

    private def mutate(probMutation: Double, items: Int) {
      val x = random.nextDouble()
      if (x <= probMutation && breedPopulation.nonEmpty) {
        val i = random.nextInt(breedPopulation.size)
        val p = random.nextInt(items)
        breedPopulation(i) = breedPopulation(i).updated(p, { if (breedPopulation(i)(p) == 0) 1 else 0 }.toByte)
      }
      ()
    }

    private def selectGene(size: Int, fitness: (List[Int], Int)): Int = {
      (0 until size)
        .foldLeft(random.nextDouble() * fitness._2)(
          (x, i) => if (x <= fitness._1(i)) return i else x - fitness._1(i)
        )
      0
    }

    private def meanFitness(totalFitness: Int, size: Int) = totalFitness / size

    private def bestSolution(fitness: List[Int]) = fitness.zipWithIndex.foldLeft(-1) {
      case (max, (f, i)) => if (f > max) i else max
    }

    private def create(size: Int, N: Int) =
      (1 to size).toList.map(
        _ =>
          List
            .fill(N)(0)
            .map(_ => {
              if (random.nextDouble() > 0.5) 1 else 0
            }.toByte)
      )

    private def evaluate(
      items:      Int,
      capacity:   Int,
      population: List[List[Byte]],
      weights:    List[Int],
      values:     List[Int]
    ) = {
      val fitness      = population.map(gene => evaluateGene(items, capacity, gene, weights, values))
      val totalFitness = fitness.sum
      (fitness, totalFitness)
    }

    private def evaluateGene(items: Int, capacity: Int, gene: List[Byte], weights: List[Int], values: List[Int]) = {
      (0 until items).foldLeft((0, 0))((r, i) => if (gene(i) == 1) (r._1 + weights(i), r._2 + values(i)) else r) match {
        case (totalWeight, totalValue) => if (capacity - totalWeight >= 0) totalValue else 0
      }
    }
  }

}
