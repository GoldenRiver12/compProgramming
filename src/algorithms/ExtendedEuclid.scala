package algorithms

import enrichMyLibrary.RichIntegralOps

object ExtendedEuclid {

  /**
   * 拡張ユークリッド互除法
   * @param a
   * @param b
   * @return  ax + by = gcd(a,b) なる整数組(x,y)
   */
  def gcdExt[T](a:T,b:T)(implicit int:Integral[T]):(T,T) ={
    import int._
    if(b == zero)
      (one,zero)
    else {
      val q = a /+ b
      val r = a %+ b
      val (x_, y_) = gcdExt(b, r)
      (y_, x_ - q * y_)
    }
  }








}


