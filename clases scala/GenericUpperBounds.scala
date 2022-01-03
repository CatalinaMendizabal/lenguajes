object Shapes:

  trait Shape:
    def area: Int

  
  case class Rect(h: Int, w: Int) extends Shape:
    override def area: Int = h * w
    def sides = 4

  case class Triangle(h: Int, w: Int) extends Shape:
    override def area: Int = (h + w) / 2
    def sides = 3

object Containers:
  
  import Shapes._
  
  case class ShapeContainer(shape: Shape)
//    def area: Int = shape.area
    
  // <: indica upper bound. El tipo T es cualquier tipo que sea subtipo de shape
  case class GenericShapeContainer[+T <: Shape](shape: T):
    def area: Int = shape.area


object GenericUpperBounds extends App:
  
  import Shapes._
  import Containers._
  
  val c1 = ShapeContainer(Rect(10,10))
  
  println(c1.shape)
  // println(c1.shape.sides)
  
  val c2 = GenericShapeContainer(Rect(10,10))
  
  println(c2.shape)
  println(c2.shape.sides)
  
  val c3 :GenericShapeContainer[Shape] = c2 // le tenes que poner que sea co-variante, porque el normal es Shape con Shape
  
  
  
  
