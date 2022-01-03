object TypeClasses:

  // definimos la type class haciendo traits, que nos obligan a implementar funciones
  // o podemos hacer que al T se le agreguen funciones
  trait SemiGroup[T]:
    extension (x: T) def combine(y: T): T
    // se puede combinar con cualquier cosa (elementos por ejemplo) ==> forma de combinar 2 en 1

  trait Monoid[T] extends SemiGroup[T]:
    def unit: T
// tiene un valor 0 o unitario

  // un monoide tiene un 0 y una funcion de combinacion
  object Monoid:
    def apply[T](using m: Monoid[T]) = m

  given Monoid[Int] with
    extension(x: Int) def combine(y: Int): Int = x + y
    def unit: Int = 0

  given Monoid[String] with
    extension(x: String) def combine(y: String): String = x + y
    def unit: String = ""

object TypeClassesUsage extends App:
  import TypeClasses.*

  def combineAll[T : Monoid](list: List[T]): T =
    list.foldLeft(Monoid[T].unit)(_.combine(_))

  println(combineAll(List(1,2,3)))
  println(combineAll(List("hola"," mundo","!")))
