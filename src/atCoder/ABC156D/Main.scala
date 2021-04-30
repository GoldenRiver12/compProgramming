package atCoder.ABC156D

object Main extends App{
  val sc = new java.util.Scanner(System.in)
  val n,a,b = sc.nextInt()

  def factorial(m:Int):BigInt ={
    (1 to m).foldLeft(BigInt(1))(_*_)
  }

  def binomial(m:Int,k:Int):BigInt={
    factorial(m)/(factorial(k)*factorial(m-k))
  }

  val mod = BigInt(10).pow(9) + 7
  val ans = (BigInt(2).pow(n) - Seq(0,a,b).map(binomial(n,_)).sum) % mod
  println(ans)
}
