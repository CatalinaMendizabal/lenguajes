package practica


/**
 *

(1) Dada una lista con datos de ventas:

     case class Sales(date: String, amounts: List[Double])

     val all = List(
      Sales("2015-05-05", List(1200, 233)),
      Sales("2015-05-06", List(3400, 24, 43)),
      ...
     )

     Implementar funciones para obtener:

   a) La lista de días en que las ventas superaron una cantidad “x” de dinero.

   b) Los días en los que se realizaron el doble de ventas que el día anterior.

   c) Los N días consecutivos en donde se obtuvieron las mejores ventas. N debe ser un parámetro de la función.

 */
case class Sales(date: String, amounts: List[Double])

object SalesApp extends App {

  val all = List(
    Sales("2015-05-05", List(1200, 233)),
    Sales("2015-05-06", List(3400, 24, 43)),
    Sales("2015-05-07", List(400, 4, 143)),
    Sales("2015-05-08", List(5030, 244, 343)),
    Sales("2015-05-09", List(30, 24, 33))
  )

  def total(s: Sales): Double = s.amounts.sum

  // (a)
  def moreThan(all: List[Sales], n: Double): Seq[Sales] = all.filter(s => total(s) > n)

  // (b)
  def doubleThanPrev(all: List[Sales]): Seq[String] = {
    all.zip(all.tail).filter {
      case (a, b) => total(b) > 2 * total(a)
    } map(_._2.date)
  }

  def bestNDays(all: List[Sales], days: Int): Seq[String] = {

    val bestSales: Seq[Sales] = all.sliding(days).toList.maxBy {
      _.map(s => total(s)).sum
    }

    bestSales map (_.date)
  }

  println(bestNDays(all, 3))
}
