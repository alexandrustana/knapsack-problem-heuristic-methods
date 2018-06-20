package hm.alg

import hm.DS

import scala.annotation.tailrec
import scala.util.Random

/**
  * @author Alexandru Stana, alexandru.stana@busymachines.com
  * @since 20/06/2018
  */
object annealing {
  import neighbourhood._

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
}
