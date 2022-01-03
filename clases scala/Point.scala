class PointV1 (_x: Int, _y: Int):
  var x = _x
  var y = _y

  override def equals(other: Any): Boolean = super.equals(other)
  override def hashCode(): Int = super.hashCode()
  override def toString(): String = s"Point($x, $y)"

case class Point(x: Int, y: Int) // case class agrega mucha funcionalidad: equals, hash, copy, toString, etc.

//  def +(p: Point) = Point(x + p.x, y + p.y)

object PointApp extends App:
  val p1 = Point(1, 2)
  val p2 = Point(3, 4)
  println(p1)
