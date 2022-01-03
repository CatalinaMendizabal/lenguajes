
def double (x: Int): Int = x * 2

@main def main (n: Int) = {
  val list = List(1,2,3)
  val new_list = list.map(double)
  println(new_list)

  def factorial0 (n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)
  def factorial (n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n - 1) // o casae x if x > 0 => x * factorial(x - 1)
  }


}

/*

0 :: list // agrego un elemento a la lista
list ::: list con tres  puntos

*/

/*
OTRA MANERA DE CORRER LA APP

object MyApp extends App {
  println(double(3))
}
*/
