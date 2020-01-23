package customml

import scala.util.Random

class Multinomial(n: Int, pLst: Seq[Double]) extends DistributionK {
    def findIdx(): Int = {
        val it = pLst.zipWithIndex.iterator
        val r = math.random
        var acc = 0.0
        while (it.hasNext) {
            val (p, idx) = it.next
            acc += p
            if (acc >= r)
                return idx
        }
        -1
    }

    def next(): Seq[Double] = {
        var arr = Array.tabulate(pLst.length)(e => 0.0)
        var count = n
        var idx = 0
        while (count > 0) {
            idx = findIdx
            arr(idx) += 1.0
            count -= 1
        }
        arr
    }
    def mean(): Seq[Double] = pLst.map(_*n)
    def variance(): Seq[Double] = pLst.map(p => n*p*(1-p))
    override def pullN(n: Int): Seq[Seq[Double]] = (1 to n).map(_ => next())
}

object Multinomial {
    def apply(n: Int, data: Seq[Seq[Double]]): Multinomial = {
        val sz = data.head.size
        val pLst = (0 to sz-1).map(i => (data.map(j => j(i)).sum) / (data.size.toDouble*n))
        new Multinomial(n, pLst)
    }
}
