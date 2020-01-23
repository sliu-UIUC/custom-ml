package customml.clustering

object KMeans {
  /**
   * Takes a sequence of N points and a k for the number of clusters.
   * Returns a sequence of k points for the centers and a sequence that maps the initial N points to the k centers.
   */
  def kMeans(data: Seq[NVect], k: Int): (Seq[NVect], Seq[Int]) = {
    val rnd = new scala.util.Random
    require(k<=data.size)
    var oldCenters = data.take(k).toArray
    var newCenters = Array.fill(k)(new NVect(Array.fill(data(0).dim)(0.0).toSeq)).toArray
    var idxs = Array.fill(data.size)(0)
    for(i <- 0 to data.size-1) {
        var minIdx  = -1
        var minDist = 1000000.0
        for(j <- 0 to k-1) {
          if(data(i).dist(oldCenters(j))<minDist) {
             minIdx = j
             minDist = data(i).dist(oldCenters(j))
          }
        }
        idxs(i) = minIdx
    }

    while(!newCenters.sameElements(oldCenters)) {
      val tmp = newCenters
      oldCenters = tmp.toList.toArray
      for(i <- 0 to k-1) {
        var currPoints  = data.filter(x=> idxs(data.indexOf(x))==i)
        var newCluster  = NVect.average(currPoints)
        newCenters(i) = newCluster
      }
      for(i <- 0 to data.size-1) {
        var minIdx  = -1
        var minDist = 1000000.0
        for(j <- 0 to newCenters.size-1) {
          if(data(i).dist(newCenters(j))<minDist) {
            minIdx = j
            minDist = data(i).dist(newCenters(j))
          }
        }
        idxs(i) = minIdx
      }

    }
    return (newCenters, idxs.toSeq)
  }

}  
