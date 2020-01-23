package customml

import scala.util.Random

/**
 * Bernoulli distribution. Add parameters to the class as needed.
 */
class Bernoulli(p: Double, rand: Random = Random) extends Distribution {
  def next(): Double = if(rand.nextDouble() < p) 1.0 else 0.0
  def mean(): Double = p
  def variance(): Double = p*(1-p)  
}

object Bernoulli {
  def apply(data: Seq[Double]): Bernoulli = {
        val p = data.count(_==1.0).toDouble / data.size.toDouble
        new Bernoulli(p, scala.util.Random)
  }
}
