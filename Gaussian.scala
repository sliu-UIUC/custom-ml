package customml

import scala.util.Random
import scala.math

class Gaussian (m:Double, va:Double)extends Distribution {
  def next(): Double = math.sqrt(va)*Random.nextGaussian()+m
  def mean(): Double = m
  def variance(): Double = va
}

object Gaussian {
  def apply(data: Seq[Double]): Gaussian = new Gaussian(BasicStats.mean(data), BasicStats.variance(data))
}

