package atCoder.ABC168D

import scala.collection.mutable

object Main extends App{
  val sc = new java.util.Scanner(System.in)
  val n = sc.nextInt()
  val m = sc.nextInt()
  val edges = Seq.fill(m)((sc.nextInt(),sc.nextInt()))


  var arrowMap = Map.empty[Int,Int]
  var arrived = Set.empty[Int]

  val graph = Graph.of((1 to n).toSet,edges)
  val queue = mutable.Queue.empty[Int]
  queue.enqueue(1)

  //var cnt=0
  arrived += 1
  while(queue.nonEmpty){
    val v = queue.dequeue()
    //cnt += graph.adjMap(v).size
    for(adj <- graph.adjMap(v) if !arrived.contains(adj)){
      queue.enqueue(adj)
      arrived += adj
      arrowMap += adj -> v
    }
  }
  //println(s"cnt:$cnt")
  println("Yes")
  for(v <- 2 to n){
    println(arrowMap(v))
  }




  class Graph[V](val vertices:Set[V],val adjMap:Map[V,Set[V]]) {
    def dfs(start:V):Seq[V] = {
      require(vertices.contains(start))
      var arrived = Set.empty[V]
      val queue = collection.mutable.Queue.empty[V]
      var ret = Seq.empty[V]

      queue.enqueue(start)
      arrived += start
      while(queue.nonEmpty){
        val v = queue.dequeue
        ret :+= v
        val next = adjMap(v).filter(!arrived.contains(_))
        queue.enqueueAll(next)
        arrived ++= next
      }
      ret
    }

    def bfs(start:V):Seq[V]={
      require(vertices.contains(start))
      var arrived = Set.empty[V]
      var ret = Seq.empty[V]

      def bfsInner(v:V):Unit ={
        arrived += v
        ret :+= v
        for(nbhd <- adjMap(v)) {
          if (!arrived.contains(nbhd)) {
            bfsInner(nbhd)
          }
        }
      }
      bfsInner(start)
      ret
    }
  }

  object Graph{
    def of[V](vertices:Set[V],edges:Seq[(V,V)]):Graph[V]={
      val edgesRev = edges.map{case (v1,v2)=>(v2,v1)}
      val adjMap = (edges ++ edgesRev).groupMap(_._1)(_._2)
        .map{case(k,v)=>(k,v.toSet)}
        .withDefaultValue(Set.empty)

      new Graph[V](vertices,adjMap)
    }
  }
}
