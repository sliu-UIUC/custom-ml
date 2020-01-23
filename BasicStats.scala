package customml

/**
 * Use population formulas instead of sample formulas for all of the places where it applies.
 */
object BasicStats {
  def mean(x: Seq[Double]): Double = x.sum/x.length
  
  def variance(x: Seq[Double]): Double = {
        val seqMean = mean(x)
        x.map(x=>math.pow(x-seqMean, 2)).sum / x.size
  } 
 
  def stdev(x: Seq[Double]): Double = math.sqrt(variance(x))
  
  def covariance(x: Seq[Double], y: Seq[Double]): Double = {
        val xMean = mean(x)
        val yMean = mean(y)
        x.zip(y).map(t=>(t._1-xMean)*(t._2-yMean)).sum / (x.size)
  }
  
  def correlation(x: Seq[Double], y: Seq[Double]): Double = {
        val xMean = mean(x)
        val yMean = mean(y)
        (x.size-1)*covariance(x,y)*(x.size)/(x.size-1)/math.sqrt(variance(x)*x.size*variance(y)*y.size)
  }
 
  def weightedMean(x: Seq[Double], weight: Double => Double): Double = x.map(t=>t*weight(t)).sum / x.map(t=>weight(t)).sum
}
