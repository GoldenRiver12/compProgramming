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

  /**
   * 剰余体 Z/pZ における逆元を求める
   * @param p
   * @param a
   * @return aの逆元（pが素数でなくaと互いに素でない場合はNone）
   */
  def invOnMod[T](p:T)(a:T)(implicit int:Integral[T]):Option[T]={
    import int._
    val (x,y) = gcdExt(p,a)
    if(p * x + a * y != one)
      None
    else {
      if(y < zero)
        Some(y+p)
      else
        Some(y)
    }
  }

}


