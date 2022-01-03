object PointConversions:
  
  // buenardo cuando usas dos bibliotecas que definen el mismo tipo
  given tupleToPoint: Conversion[(Int, Int), Point] = t => Point(t._1, t._2)

  given intToPoint: Conversion[Int, Point] = v => Point(v, v)


object GivenConversions extends App:

  import scala.language.implicitConversions
  import PointConversions.given

  def helloPoint(p: Point): Unit =
    println(p)

  val p: Point = Point(2,3)

  val t = (3, 4)
  val n: Point = (3, 4)

  val z: Point = 0

  helloPoint(t)
  helloPoint(0)

