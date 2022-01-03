import Objects.Dimension

object Arithmetics:

  trait Adder[T]:
    def zero: T
    def add(a: T, b: T): T

  object Adder:
    def apply[A](using v: Adder[A]): Adder[A] = v // apply son los parentesis
//    def xyz[A](using v: Adder[A]): Adder[A] = v

  def sum0(list: List[Int]): Int =
    list.foldLeft(0)(_ + _)

  // parametros implicitos con el using
  def sum1[T](list: List[T])(using adder: Adder[T]) =
    list.foldLeft(adder.zero)(adder.add)

  // context bound [T : Adder] -> Debe existir un adder para T
  def sum2[T : Adder](list: List[T]) =
    val adder = summon[Adder[T]] // summon the all mighty one
    list.foldLeft(adder.zero)(adder.add)

  def sum3[T : Adder](list: List[T]) =
//    val adder = Adder.xyz
    list.foldLeft(Adder[T].zero)(Adder[T].add) // Adder[T] llama al .apply
    // version basica de type classes

  given intAdder: Adder[Int] = new Adder[Int] :
    def zero = 0
    def add(a:Int, b:Int): Int = a + b

object Objects:
  case class Dimension(h: Int, w: Int)

  given Ordering[Dimension] = Ordering.by(e => e.h)

  given numeric: Numeric[Dimension] = new Numeric[Dimension]:
    def plus(x: Dimension, y: Dimension): Dimension = Dimension(0,0)
    def minus(x: Dimension, y: Dimension): Dimension = Dimension(0,0)
    def times(x: Dimension, y: Dimension): Dimension = Dimension(0,0)
    def negate(x: Dimension): Dimension = Dimension(0,0)
    def fromInt(x: Int): Dimension = Dimension(0,0)
    def parseString(str: String): Option[Dimension] = None
    def toInt(x: Dimension): Int = 0
    def toLong(x: Dimension): Long = 0
    def toFloat(x: Dimension): Float = 0
    def toDouble(x: Dimension): Double = 0

    override def compare(x: Dimension, y: Dimension): Int = 0


object GenericsContextBound:

  import Arithmetics.{given, *}
  import Objects.*

  val numbers = List(1, 2, 3)
  // esto funciona por type classes nada más
  numbers.sum
  numbers.max
  numbers.sorted

  sum0(numbers)
  sum1(numbers)
  sum2(numbers)

  given Adder[Point] = new Adder[Point] : // se puede no darle nombre
    def zero = Point(0,0)
    def add(a: Point, b: Point): Point = a + b

  val p = Point(3, 4)
  sum1(List(p,p))

  val dims = List(Dimension(23, 4), Dimension(2, 3))
  dims.sum
  dims.sorted
