package object EnrichMyLibrary {
  implicit class RichIntegralOps[T](val t1:T)(implicit int:Integral[T]){
    import int._
    def /+(t2:T):T ={
      if(t1 % t2 >= zero)
        t1 / t2
      else
        t1 / t2 - t2.sign
    }
    def %+(t2:T):T ={
      val mod = t1 % t2
      if(mod >= zero)
        mod
      else
        mod + t2.abs
    }
  }
}
