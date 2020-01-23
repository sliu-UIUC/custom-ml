package customml.parametricmethods

import scala.math
/**
 * This is an implementation of the 1D parametric classifier. The input is a sequence of
 * x and r values. Because it is 1D, here is only one Double for the x and the r is
 * a one-hot encoded sequence.
 */
class Classifier1D(xr: Seq[(Double, Seq[Double])]) {

  /**
   * Given a value for x, return one-hot encoding for the class that it belong in.
   */
  def classify(x: Double): Seq[Double] = {
	val dists    = xr.map(t => math.abs(t._1-x))
	val classNum = dists.indexOf(dists.min)
	xr(classNum)._2
  }
}

object Classifier1D {
  /**
   * This is an alternate way of making a classifier where the r values are not
   * one-hot encoded. They can be whatever type you want, it should just be a fairly small
   * set of value. The result is both a classifier and a lookup table to let the user
   * know which value of type A corresponds with each class in the one-hot encoded
   * r values that are spit out by the classifier.
   */
  def apply[A](xAndClass: Seq[(Double, A)]): (Classifier1D, Seq[A]) = {
	val classes = xAndClass.map(_._2).distinct
	var ret:Seq[(Double, Seq[Double])] = Seq()
	for (i <- 0 to classes.size-1) {
		val currArr  = xAndClass.filter(_._2==classes(i)).map(_._1)
		val currMean = currArr.sum / currArr.size
		var oneHotSq =  Array.fill(classes.size)(0.0)
		oneHotSq(i)  = 1.0 
		ret :+= (currMean, oneHotSq.toSeq)
	}
	//(new Classifier1D(Seq[(Double, Seq[double])]), Seq[A] )
	(new Classifier1D(ret), classes)
  }
}
