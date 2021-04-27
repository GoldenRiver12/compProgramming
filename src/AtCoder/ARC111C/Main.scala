package AtCoder.ARC111C
object Main extends App{
  val sc = new java.util.Scanner(System.in)
/*  val n = 200000
  val weights = -1 +: Seq.fill(n)(1000000000).toIndexedSeq
  val itemWeights = -1 +: Seq.fill(n)(Random.nextInt(100000000)).toIndexedSeq
  val items:Seq[Int] = -1 +: Random.shuffle((1 to n).toList).toIndexedSeq*/
  val n = sc.nextInt()
  val weights = -1 +: Seq.fill(n)(sc.nextInt()).toIndexedSeq
  val itemWeights = -1 +: Seq.fill(n)(sc.nextInt()).toIndexedSeq
  val items = -1 +: Seq.fill(n)(sc.nextInt()).toIndexedSeq
  var ownerInfo = Map.empty[Int,Person]

  val itemList:IndexedSeq[Item] = null +: (for(i <- 1 to n)yield{

    new Item(i,itemWeights(i),null)
    //new Item(1,1,null)
  })
  val personList:IndexedSeq[Person] = for(i <- 1 to n) yield{
    val item = itemList(items(i))
    val person = new Person(i,weights(i),itemList(items(i)))
    item.owner = person
    ownerInfo += item.no -> person
    person
  }
  val sortedPersonList = personList.sortBy(_.weight)
/*  println("items:")
  for(i <- itemList.tail){
    println(i.no + ":" + i.itemWeight)
  }
  println("persons:")
  for(p <- sortedPersonList){
    println(p.no + ":" + p.weight + ":" + p.item.no)
  }*/

  val ans:Option[Seq[(Int,Int)]] = if(sortedPersonList.exists(p => p.no != p.item.no && p.weight <= p.item.itemWeight)){
    None
  }else{
    Some(for(person <- sortedPersonList if person.no != person.item.no)yield{
      val swapper1 = person
      val swapper2 = ownerInfo(person.no)
      swapper1 swapItem swapper2
      (swapper1.no,swapper2.no)
    })
  }

  val ansStr = ans match{
    case None => "-1"
    case Some(ansSome) => ansSome.size + "\n" + ansSome.map{case(a,b) => a + " "+b+"\n"}.mkString("")
  }

  println(ansStr)


  class Person(val no:Int,val weight:Int,var item:Item){
    def swapItem(other:Person):Unit = {
      val item1 = item
      val item2 = other.item
      item = item2
      other.item = item1
      item1.owner = other
      item2.owner = this
      ownerInfo += item1.no -> other
      ownerInfo += item2.no -> this
    }
  }
  class Item(val no:Int,val itemWeight:Int,var owner:Person)
}
