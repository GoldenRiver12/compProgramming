import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.io.StdIn
import scala.util.Random
import scala.collection.mutable.{Map => MMap}
import scala.util.control.Breaks

object Main extends App{
  val sc = new java.util.Scanner(System.in)
  val debugMode = true
  val dl = new DebugLogger(debugMode)


  val n = sc.nextInt()
  val towers = Seq.fill(n)(sc.nextLong())
  val mod = BigInt.apply(10).pow(9) + 7

  val towersSorted = towers.sorted
  var num = 0L
  val towersDistinct = for(t <- towersSorted if t != num)yield{
    val ret = t - num + 1
    num = t
    ret
  }

  val ans = towersDistinct.foldLeft(BigInt(1)){_ * _ % mod}
  println(ans)













  def printExecTime(process: => Unit): Unit = {
    val start = System.currentTimeMillis
    process
    println("処理時間： " + (System.currentTimeMillis - start) + " ミリ秒")
  }

  class DebugLogger(val debugMode:Boolean){
    def log(s:Object):Unit={
      if(debugMode)
        println(s.toString)
    }

  }
}