package dataStructure

class UnionFindTree[T](val domain:Set[T]){
  var parentMap:Map[T,Option[T]] = domain.map(_ -> None).toMap
  var rankMap:Map[T,Int] = domain.map(_ -> 0).toMap
  def getRep(t:T):T = {
    require(domain contains t)
    parentMap(t) match{
      case None => t
      case Some(t2) =>
        val rep = getRep(t2)
        parentMap += t -> Some(rep)
        rep

    }
    //parentMap(t).map(getRep).getOrElse(t)
  }

  def unite(t1:T, t2:T):Unit ={
    require(domain contains t1)
    require(domain contains t2)
    if(!inSameClass(t1,t2))
      link(t1,t2)
  }

  def link(t1:T,t2:T):Unit ={
    require(domain contains t1)
    require(domain contains t2)
    if(rankMap(t1) > rankMap(t2)){
      parentMap += t2 -> Some(t1)
    }else{
      parentMap += t1 -> Some(t2)
      if(rankMap(t1) == rankMap(t2))
        rankMap += t2 -> (rankMap(t2) + 1)
    }

  }

  def inSameClass(t1:T,t2:T):Boolean ={
    require(domain contains t1)
    require(domain contains t2)
    getRep(t1) == getRep(t2)
  }
}

