// Dibujo del triangulito xd -> xDate
// VA A TOMAR TYPE CLASS DIJO EL DON
object TypeClasses:
  // https://typelevel.org/cats/
  // https://zio.dev/

  // definimos la type class haciendo traits, que nos obligan a implementar funciones
  // o podemos hacer que al T se le agreguen funciones
  trait SemiGroup[T]: // algo que se puede combinar
    extension (x: T) def combine(y: T): T // hace que lo pueda llamar haciendo x.combine(y)

  trait Monoid[T] extends SemiGroup[T]:
    def unit: T
  // semigroup y monoid son type classes.

  // un monoide tiene un 0 y una funcion de combinacion
  object Monoid:
    def apply[T](using m: Monoid[T]) = m

  // instancias predefinidas de la type class
  given Monoid[Int] with
    extension(x: Int) def combine(y: Int): Int = x + y
    def unit: Int = 0

  given Monoid[String] with
    extension(x: String) def combine(y: String): String = x + y
    def unit: String = ""

object TypeClassesUsage extends App:
  import TypeClasses.*

  /*
  defino una funcion generica, para cualquier tipo T del que exista un monoid. Va a funcionar con
  bibliotecas que todavia no existen y para las que ya existen y no eran monoid.
  */
  def combineAll[T : Monoid](list: List[T]): T =
    list.foldLeft(Monoid[T].unit)(_.combine(_))

  println(combineAll(List(1,2,3)))
  println(combineAll(List("hola"," mundo","!")))