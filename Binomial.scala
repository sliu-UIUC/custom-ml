package customml

import scala.util.Random

class Binomial(n: Int,p:Double, rand: Random = Random) extends Distribution {
  def next(): Double = (1 to n).map(_ => if(math.random < p) 1.0 else 0.0).sum
  def mean(): Double = n*p
  def variance(): Double = n*p*(1-p)
}

object Binomial {
  def apply(n: Int, data: Seq[Double]): Binomial = {
	val p = data.sum/data.length/n
        new Binomial(n,p, scala.util.Random)
  }
}
