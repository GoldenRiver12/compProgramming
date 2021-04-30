package algorithms

object MaximumSubarray {

  /**
   * 尺取り法を用いて，総和が最大となる連続部分数列の総和を求める
   * @param ary
   * @param numeric
   * @tparam T
   * @return
   */
  def getSumOfMaximumSubarray[T](ary:Seq[T])(implicit numeric: Numeric[T]):T = {
    require(ary.nonEmpty)
    import numeric._
    var maxSoFar = ary.head
    var maxEndingHere = ary.head
    for(n <- ary.tail){
      maxEndingHere = n max (maxEndingHere + n)
      maxSoFar = maxSoFar max maxEndingHere
    }
    maxSoFar
  }
}
