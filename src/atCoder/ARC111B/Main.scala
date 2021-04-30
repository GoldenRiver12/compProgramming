package atCoder.ARC111B

object Main extends App {
  val sc = new java.util.Scanner(System.in)
  val n = sc.nextInt()
  val cards = Seq.fill(n)(Set(sc.nextInt(),sc.nextInt()))
  //println(cards)

  var colorCntMap = Map.empty[Int,Int].withDefaultValue(0)
  for(card <- cards;
      color <- card){
    colorCntMap += color -> (colorCntMap(color) + 1)
  }

  //println(colorCntMap)

  var selectedColors = Set[Int]()
  for(card <- cards){
    if(card.exists(!selectedColors.contains(_))) {
      val selectedColor = card.filter(!selectedColors(_)).minBy(colorCntMap(_))
      selectedColors += selectedColor

    }
    card.foreach(c => colorCntMap += c -> (colorCntMap(c) - 1))

  }

  val ans = selectedColors.size
  println(ans)


}
