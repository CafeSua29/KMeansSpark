import scala.io.Source
import scala.util.Random

class Point(val dx: Int, val dy: Int) {
  private var cluster : Point = _
  def setCluster(p : Point) : Unit = {
    cluster = p
  }
  def getCluster: Point = {
    return cluster
  }
  
  def distance(p : Point) : Double = {
    val xx = p.dx - dx
    val yy = p.dy - dy
    return math.sqrt(xx * xx + yy * yy)
  }
  
  def newAverage(points : List[Point]) : Point = {
    var newX = 0
    var newY = 0
    for(p <- points){
      newX = newX + p.dx
      newY = newY + p.dy
    }
    newX = newX/points.length
    newY = newY/points.length
    return new Point(newX, newY)
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

  override def equals(other: Any): Boolean = {
    other match {
      case that: Point =>
        that.canEqual(this) &&
          dx == that.dx &&
          dy == that.dy
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val state = Seq(dx, dy)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString = s"Point($dx,$dy)"
}

object Main extends App {
  private val K = 3
  private val points = readPoint("output4.txt")
  private var clusters = selectRandomStart()

  private def readPoint(fileName : String) : List[Point] = {
    val bufferedSource = Source.fromFile(fileName)
    val points = bufferedSource.getLines().map { line =>
      val coordinates = line.split(",").map(_.trim.toInt)
      new Point(coordinates(0), coordinates(1))
    }.toList
    bufferedSource.close()
    points
  }

  private def selectRandomStart() : List[Point] = {
    Random.shuffle(points).take(K)
  }

  private def assignClusters(): Unit = {
    for (p <- points) {
      val nearestCluster = clusters.minBy(c => p.distance(c))
      p.setCluster(nearestCluster)
    }
  }

  private def updateCluster() : Boolean = {
    var check = false
    clusters = clusters.map { c =>
      val newc = c.newAverage(points.filter(_.getCluster.equals(c)))
      if (!newc.equals(c)) {
        check = true
        newc
      } else {
        c
      }
    }
    for(c <- clusters){
      print(c.toString + " ")
    }
    print("\n")
    return check
  }

  private var checked = true
  while (checked){
    assignClusters()
    checked = updateCluster()
  }
}