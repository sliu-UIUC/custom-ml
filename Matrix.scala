package customml

class Matrix(private val values: Seq[Seq[Double]]) {
  def rows: Int = values.size
  def columns: Int = values(0).size
  def apply(row: Int, col: Int): Double = values(row)(col)

  def +(that: Matrix): Matrix = {
    require(rows == that.rows && columns == that.columns)
    //values.zip(that).map(i=> i._1.zip(i._2)).map(i=>i.map(x=>x._1+x._2))
    var ret = Array.fill(rows)(Array.fill(columns)(0.0))
    for(i <- 0 to rows-1) {
      for(j<- 0 to columns-1) {
	ret(i)(j) = values(i)(j)+that(i, j)
      }
    }
    new Matrix(ret.map(_.toSeq).toSeq)
  }
  def -(that: Matrix): Matrix = {
    require(rows == that.rows && columns == that.columns)
    var ret = Array.fill(rows)(Array.fill(columns)(0.0))
    for(i <- 0 to rows-1) {
      for(j<- 0 to columns-1) {
        ret(i)(j) = values(i)(j)-that(i, j)
      }
    }
    new Matrix(ret.map(_.toSeq).toSeq)
  }
  def *(that: Matrix): Matrix = {
    require(columns == that.rows)
    val tt = that.values.transpose
    var ret = Array.fill(rows)(Array.fill(that.columns)(0.0))
    for(i <- 0 to rows-1) {
      for(j <- 0 to that.columns-1) {
        ret(i)(j) = values(i).zip(tt(j)).map(x=>x._1*x._2).sum
      }
    }
    new Matrix(ret.map(_.toSeq).toSeq)
  }
  def *(x: Double): Matrix = new Matrix(values.map(r=>r.map(_*x)))  
  
  def det: Double = {
    require(rows == columns)
    var a = values.map(_.toArray).toArray
    var det = 1.0
    for(i <- 0 to rows-1){
      var k = i
      for (j <- i+1 to rows-1){
	if(math.abs(a(j)(i))>math.abs(a(k)(i))) k = j
      }
      if(math.abs(a(k)(i))<0.0000000001) {
	det =0.0
	return det
      }
      var tmp = a(i)
      a(i) = a(k)
      a(k) = tmp 
      if (i!=k) det = -det
      det = det*a(i)(i)
      for(j <- i+1 to rows-1) a(i)(j) /=a(i)(i)
      for(j <- 0 to rows-1){
	if(j!=i && math.abs(a(j)(i))>0.000000001) {
	  for(k <- i+1 to rows-1) {
	    a(j)(k) -=a(i)(k)*a(j)(i)
	  }
	}
      }
    }
    math.round(det).toDouble
  }
  override def toString: String = values.map(_.mkString("|", " ", "|")).mkString("\n")
}

object Matrix {
  def apply(values: Seq[Seq[Double]]): Matrix = new Matrix(values)
}
