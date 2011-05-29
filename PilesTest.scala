class Point3 {
  var (x, y, z) = (0, 0, 0)

  def print {
    println("%d %d %d".format(x, y, z))
  }
}

object PilesTest extends App {
  val n = 10000000

  def time(func:  => Unit) : Long = {
    val n = 10
    func // JIT warmup
    val then = System.currentTimeMillis
    for(i <- 0 until n)
      func
    val timeDiff = System.currentTimeMillis - then
    printf("%fms\n", timeDiff.toDouble / n.toDouble)
    return timeDiff
  }

  var point: Point3 = null

  val point3PileCls = Piles.genPile(classOf[Point3])
  val point3PiledCls = Piles.genPiled(classOf[Point3], point3PileCls)

  var pile = point3PileCls.newInstance.asInstanceOf[Pile]
  var piled = point3PiledCls.newInstance.asInstanceOf[Piled]
  point = piled.asInstanceOf[Point3]

  pile.init(n)
  piled.setPile(pile)

  val rt = Runtime.getRuntime
  println("mem: %d".format(rt.totalMemory - rt.freeMemory))

  time {
    for(i <- 0 until n) {
      piled.setPileIdx(i)
      point.x = i
      point.y = i * 2
    }
  }

  pile = null
  piled = null
  point = null

  val points = new Array[Point3](n)
  (0 until n).foreach(points(_) = new Point3)

  println("mem: %d".format(rt.totalMemory - rt.freeMemory))

  time {
    for(i <- 0 until n) {
      val p = points(i)
      p.x = i
      p.y = i * 2
    }
  }

}
