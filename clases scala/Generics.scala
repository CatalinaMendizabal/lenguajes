object Generics extends App:
  // T es un parametro invariante
  // +T es un parametro co-variante
  // -T es un parametro contravariante
  sealed abstract class Tree[+T] // El "+" permite guardar el tipo de dato a uno de mayor jerarquia (*)

  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  case class Leaf[T](x: T) extends Tree[T]

  trait Printer[-T]:
    def print(value: T): Unit

  class StringPrinter extends Printer[String]:
    override def print(value: String): Unit = println(value)

  class IntPrinter extends Printer[Int]:
    override def print(value: Int): Unit = println(value)

  class AnyPrinter extends Printer[Any]:
    override def print(value: Any): Unit = println(value)

  val tree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(4)))
  val tree2: Tree[Any] = tree // (*)

  val list: List[Int] = List(1,2,3)
  val anyList: List[Any] = list

  val s: java.lang.String = "12345"
  s.filter(_ != '4')

  println(tree)

  val sp = new StringPrinter

  def printString(value: String, printer: Printer[String]) =
    printer.print(value)

  sp.print("test")
  
  printString("hello", new StringPrinter)
  printString("hello", new AnyPrinter)