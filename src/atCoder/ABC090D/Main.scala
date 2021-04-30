package atCoder.ABC090D

object Main extends App {
  val sc = new java.util.Scanner(System.in)
  val n,k = sc.nextLong()
  val ans = (for(b <- 1 to n.toInt)yield{
    val subAns = if(b <= k)
      0L
    else{
      val q = n / b
      val m = n % b
      val main = ((b - k) max 0)* q + (if(k == 0) -1 else 0)
      val rem = (m - k + 1) max 0
      //println(b,main + rem ,main,rem,q,m)
      main + rem
    }
    subAns
  })
    .sum
  println(ans)
}
