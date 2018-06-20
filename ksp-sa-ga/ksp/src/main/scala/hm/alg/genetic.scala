package hm.alg

import hm.DS

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * @author Alexandru Stana, alexandru.stana@busymachines.com
  * @since 20/06/2018
  */
object genetic {
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
