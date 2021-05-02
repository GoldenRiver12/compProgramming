package dataStructure

import scala.util.Random

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

  /**
   * 完全グラフを返す
   * @param vertices
   * @tparam V
   * @return
   */
  def completeGraphOf[V](vertices:Set[V]):Graph[V] ={
    of(vertices, vertices.toSeq.combinations(2).map{case a +: b +: _ =>(a,b)}.toSeq)
  }

  /**
   * 与えられた頂点集合にランダムに辺を加えたグラフを返す
   * @param vertices
   * @param density 各頂点間に辺が張られる確率(0 <= _ <= 1)
   * @tparam V
   * @return
   */
  def generateRandomGraph[V](vertices:Set[V], density:Double):Graph[V]={
    require(density >= 0 && density <= 1)
    val rng = new Random()
    val edges = vertices.toSeq.combinations(2)
      .filter(_ => rng.nextDouble() <= density)
      .map{case Seq(a,b,_*) =>(a,b)}
      .toSeq

    of(vertices, edges)
  }

  /**
   * 与えられた頂点集合にランダムに辺を加えた連結グラフを返す
   * @param vertices
   * @param density 各頂点間に辺が張られる確率(0 <= _ <= 1)
   * @param isDense 密グラフとして生成するか（trueなら計算量は頂点数の2乗に比例，falseなら生成する辺数に比例）
   * @tparam V
   * @return 連結グラフ（density=0でも連結のための最低限の辺は張られる）
   */
  def generateRandomConnectedGraph[V](vertices:Set[V], density:Double,isDense:Boolean = true):Graph[V]={
    require(density >= 0 && density <= 1)
    val rng = new Random()
    //全頂点を通る単純道
    val path:Set[(V,V)] = rng.shuffle(vertices.toSeq)
      .sliding(2)
      .map{case Seq(a,b,_*) =>(a,b)}
      .toSet

    val edges = {
      if(isDense)
        vertices.toSeq.combinations(2)
          .filter(_ => rng.nextDouble() <= density)
          .map{case Seq(a,b,_*) =>(a,b)}
          .toSet
      else{
        val verticesSeq = vertices.toSeq
        (for(_ <- 1 to (vertices.size * vertices.size * density).toInt)yield {
          (verticesSeq(rng.nextInt(verticesSeq.size))
            ,verticesSeq(rng.nextInt(verticesSeq.size)))
        }).toSet
      }
    }

    //ランダムに張った辺に道を加えることで連結にする
    of(vertices,edges.union(path).toSeq)
  }
}
