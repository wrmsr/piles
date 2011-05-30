class Point3 {
  //\/ this apparently holds onto a tuple3 instance in each class instance :o avoid for efficiency
  //var (x, y, z) = (0, 0, 0)

  var x = 0
  var y = 0
  var z = 0

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

  val rt = Runtime.getRuntime

  def printMem {
    rt.gc
    println("mem: %d".format(rt.totalMemory - rt.freeMemory))
    println
  }

  def testPiled {
    println("Piled:")

    val point3PileCls = Piles.genPile(classOf[Point3])
    val point3PiledCls = Piles.genPiled(classOf[Point3], point3PileCls)

    val pile = point3PileCls.newInstance.asInstanceOf[Pile]
    val piled = point3PiledCls.newInstance.asInstanceOf[Piled]
    val point = piled.asInstanceOf[Point3]

    pile.init(n)
    piled.setPile(pile)

    time {
      for(i <- 0 until n) {
        piled.setPileIdx(i)
        point.x = i
        point.y = i * 2
      }
    }

    printMem
  }

  def testPile {
    println("Pile:")

    val openPoint3PileCls = Piles.genPile(classOf[OpenPoint3])

    val pile = openPoint3PileCls.newInstance.asInstanceOf[Pile]
    val point = new OpenPoint3

    pile.init(n)

    time {
      for(i <- 0 until n) {
        pile.load(point, i)
        point.x = i
        point.y = i * 2
        pile.store(point, i)
      }
    }

    printMem
  }

  def testArr {
    println("Array:")

    val points = new Array[Point3](n)
    (0 until n).foreach(points(_) = new Point3)

    time {
      for(i <- 0 until n) {
        val point = points(i)
        point.x = i
        point.y = i * 2
      }
    }

    printMem
  }

  testPiled
  testPile
  testArr
}
