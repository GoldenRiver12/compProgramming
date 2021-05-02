package atCoder.ABC168D

import scala.collection.mutable
import scala.util.Random

object Main extends App{
  val sc = new java.util.Scanner(System.in)
  val debugMode = false
  val dl = new DebugLogger(debugMode)



  val n = sc.nextInt()
  val m = sc.nextInt()

  Timer.reset()

  val edges = mutable.ArrayBuffer.fill(m+3)(mutable.ArrayBuffer.empty[Int])
  for(_ <- 1 to m){
    val v1,v2 = sc.nextInt()
    edges(v1).addOne(v2)
    edges(v2).addOne(v1)
  }

  dl.log(Timer.getExecTimeMsg())

  var arrowMap = mutable.Seq.fill(n+3)(-1)
  var arrived = mutable.Seq.fill(n+3)(false)

  val graph = Graph(1 to n, edges, edges(_))


  val queue = mutable.Queue.empty[Int]
  queue.enqueue(1)

  dl.log(Timer.getExecTimeMsg())

  var cnt=0
  arrived.update(1,true)
  while(queue.nonEmpty){
    val v = queue.dequeue()
    cnt += graph.getAdj(v).size
    for(adj <- graph.getAdj(v) if !arrived(adj)){
      queue.enqueue(adj)
      arrived.update(adj,true)
      arrowMap.update(adj,v)
    }
  }
  dl.log(s"cnt:$cnt")

  dl.log(Timer.getExecTimeMsg())

  println("Yes")
  for(v <- 2 to n){
    println(arrowMap(v))
  }


  dl.log(Timer.getExecTimeMsg())

  case class Graph[V,E](vertices:Iterable[V],edges:E, adj:V => Iterable[V]) {
    def getAdj(v:V):Iterable[V]={
      adj(v)
    }
  }



  object Timer{
    val main:String = new Throwable().getStackTrace()(1).getMethodName
    var time:Long = System.currentTimeMillis()

    def reset():Unit={
      time = System.currentTimeMillis()
    }

    def getExecTimeMsg(name:String = ""):String ={
      val lineNo = new Throwable()
        .getStackTrace
        .filter(_.getMethodName == main).head
        .getLineNumber
      s"$name 処理時間： $getExecTime ミリ秒 （行 : $lineNo）"
    }

    def getExecTime:Long={
      val ret = System.currentTimeMillis - time
      reset()
      ret
    }
  }

  class DebugLogger(val debugMode:Boolean){
    def log(s:Any):Unit={
      if(debugMode)
        println(s.toString)
    }

  }
}
