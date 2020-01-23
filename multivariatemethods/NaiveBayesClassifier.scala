package customml.multivariatemethods

class NaiveBayesClassifier(xr: Seq[(Seq[Double], Seq[Double])]) {

  def classify(x: Seq[Double]): Seq[Double] = {
  	val dists = xr.map(t=> math.sqrt((t._1, x).zipped.map((y,z) => math.abs(y-z)).foldLeft(0.0)((acc, i)=> acc + math.pow(i,2))))
	val classNum = dists.indexOf(dists.min)
        xr(classNum)._2
  }
}

object NaiveBayesClassifier {
  def apply[A](xAndClass: Seq[(Seq[Double], A)]): (NaiveBayesClassifier, Seq[A]) = {
	val classes = xAndClass.map(_._2).distinct
	var ret:Seq[(Seq[Double], Seq[Double])] = Seq()
	for (i <- 0 to classes.size-1) {
		val currArr  = xAndClass.filter(_._2==classes(i)).map(_._1)/*.map(t=> t._1.flatMap {
			       x =>
				 x match {
				   case x: Int => Some(x.toDouble)
     				   case x: Double => Some(x)
      				   case _ => None
     				}
  		})*/
		val currTupleMean =  currArr.transpose.map(x=> x.sum/x.size)
		var oneHotEq      =  Array.fill(classes.size)(0.0)
		oneHotEq(i)       = 1.0
		ret :+= (currTupleMean, oneHotEq.toSeq)
	}
	(new NaiveBayesClassifier(ret), classes)
  }
}
