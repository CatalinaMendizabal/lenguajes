import scala.io.Source
/*
 * Final Ej Scala Catalina Mendizabal
*/

case class Venta(provincia: String, distrito: String, producto: String, cantidad: Int)

def loadFile(file: String): List[Venta] =
  val source = Source.fromFile(file).mkString
  val sales = file.split('\n').flatMap(_.split(',')).map(_.trim).toList.sliding(4, 4).filter(_.length == 4).toList
  sales.map {
    case List(prov, dist, prod, cant) =>
      Venta(prov, dist, prod, cant.toInt)
  }

// Cantidad total de productos vendidos
def totalSold(sales: List[Venta]): Int = sales.map(_.cantidad).sum

// El producto mas vendido
def bestSellerProduct(sales: List[Venta]): String =
  val (product, ammount) = sales.groupBy(_.producto).map {
    case (product, productList) => product -> totalSold(productList)
  }.toList.maxBy(_._2)
  product

// la provincia en la cual obtuvo mejores ventas un producto pasado como parámetro
def bestSellerProductProvince(product: String, sales: List[Venta]) =
    sales
    .filter(_.producto == product)
    .groupBy(_.provincia)
    .map { case (prov, saleList) => prov -> totalSold(saleList) }
    .toList
    .maxBy(_._2)._1

