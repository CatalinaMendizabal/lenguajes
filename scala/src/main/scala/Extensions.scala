object PointLib :
  extension (p: Point)
    def invert = Point(p.y, p.x)

  extension (s: String)
    def wow = s.toUpperCase() + "!!!"


@main def extensions =
  import PointLib.*

  val p = Point(3,5)

  println(p.invert)
  println("hello".wow)