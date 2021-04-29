import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.io.StdIn
import scala.util.Random
import scala.collection.mutable.{Map => MMap}
import scala.util.control.Breaks
import Algorithms.ExtendedEuclid

object Main extends App{
  val sc = new java.util.Scanner(System.in)
  val debugMode = true
  val dl = new DebugLogger(debugMode)

  println(ExtendedEuclid.gcdExt(-10,-3))



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