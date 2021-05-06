package aizuOnlineJudge.GRL_1_B

import scala.collection.mutable

object Main extends App{
  val sc = new java.util.Scanner(System.in)

  val cardV = sc.nextInt
  val cardE = sc.nextInt
  val r = sc.nextInt

  val edges = Seq.fill(cardE)(Edge(sc.nextInt,sc.nextInt,sc.nextInt))

  val distUpper:mutable.IndexedSeq[IntExt] = mutable.IndexedSeq.fill(cardV)(PosInfty())
  distUpper(r) = 0

  for(_ <- 1 until cardV){
    for(e <- edges){
      relax(e)
    }
  }

  val hasNegLoop =
    edges.exists{case Edge(head,tail,weight) => distUpper(tail) > distUpper(head) + weight}

  if(hasNegLoop){
    println("NEGATIVE CYCLE")
  }else{
    for(v <- 0 until cardV){
      val minDist = distUpper(v)
      println(if(minDist.isInfty) "INF" else minDist)
    }
  }


  def relax(e:Edge):Unit ={
    if(distUpper(e.tail) > distUpper(e.head) + e.weight)
      distUpper(e.tail) = distUpper(e.head) + e.weight

  }

  trait IntExt{
    def +(that:IntExt):IntExt ={
      (this,that)match{
        case(RowInt(i),RowInt(j)) => i + j
        case _ => PosInfty()
      }
    }

    def >(that:IntExt):Boolean ={
      (this,that)match{
        case(RowInt(i),RowInt(j)) => i > j
        case(PosInfty(),RowInt(_)) => true
        case(RowInt(_),PosInfty()) => false
        case(PosInfty(),PosInfty()) => true
      }
    }

    def <(that:IntExt):Boolean = !(this > that) && this != that

    def isInfty:Boolean = this.isInstanceOf[PosInfty]
    def nonInfty:Boolean = !isInfty

    override def toString: String =
      this match{
        case RowInt(i) => i.toString
      }
  }

  case class RowInt(i:Int) extends IntExt{
    override def toString: String = i.toString
  }
  implicit def IntToRowInt(i:Int):RowInt =RowInt(i)
  case class PosInfty() extends IntExt{
    override def toString: String = "INF"
  }

  case class Edge(head:Int, tail:Int, weight:Int)
}
