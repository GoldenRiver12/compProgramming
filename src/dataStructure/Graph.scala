package dataStructure

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
