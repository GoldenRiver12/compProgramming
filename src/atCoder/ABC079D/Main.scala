package atCoder.ABC079D
object Main extends App{
  val sc = new java.util.Scanner(System.in)
  val debugMode = true
  val dl = new DebugLogger(debugMode)

  type Matrix[K] = IndexedSeq[IndexedSeq[K]]

  val height,width = sc.nextInt()
  val vertices :IndexedSeq[Int]= 0 to 9
  val weightMatrix :Matrix[Int] =
    for(_ <- 0 to 9)yield
      for(_ <- 0 to 9)yield
        sc.nextInt()

  val wall =IndexedSeq.fill(height)(IndexedSeq.fill(width)(sc.nextInt())).flatten.filter(_ != -1)


  var w:Matrix[Int] = weightMatrix
  for(c <- 0 to 9){
    w =
      for (i <- 0 to 9)yield
        for(j <- 0 to 9)yield
          w(i)(j) min (w(i)(c) + w(c)(j))
  }

  //println(w.map(_.mkString(" ")).mkString("\n"))
  val ans = wall.map(w(_)(1)).sum
  println(ans)

  class DebugLogger(val debugMode:Boolean){
    def log(s:Any):Unit={
      if(debugMode)
        println(s.toString)
    }

  }
}

