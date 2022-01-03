case class Point(x: Int, y: Int): // case class agrega mucha funcionalidad: equals, hash, copy, toString, etc.

  def +(p: Point) = Point(x + p.x, y + p.y)
