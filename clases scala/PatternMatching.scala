object PatternMatching extends App:
  case class Invoice(number: Int, client: Client, items: List[Item])
  case class Client(name: String, address: Address)
  case class Address(street: String, loc: String)

  case class Item(id: Int, description: String, price: Double)

  val list = List(1,2,3)

  //  if(list.isInstanceOf[List[Int]]){
  //    val initList = list.asInstanceOf[List[Int]]
  //
  //    if(initList.length != 3)
  //      throw MatchError()
  //
  //    val a: Int= initList(0)
  //    val b: Int= initList(1)
  //    val c: Int= initList(2)
  //  }
  // Esto se resuelve con lo de abajo muy rico

  val List(a, b, c) = list

  val c1 = Client("Juan", Address("Rivadavia 200", "CABA"))

  val Client(name, Address(_, loc)) = c1

  //  //faltaria el instanceof
  //  val name = c1.name
  //  val loc = c1.address.loc

  def rate(c: Client): Double =
    c match
      case Client(_, Address(_, "CABA")) => 10.0
      case _ => 12.0

  val clientName = "Juan"
  val msg = s"Hola $clientName"

  msg match
    case s"Hola $name" => println(s"name: $name")
    case other => println(s"unknown pattern: $other")

  val tuple = (10, true, "hello")
  val (n, bool, s) = tuple

  val elements = List(true, 3, c1)

  elements.foreach {
    case b: Boolean => println("boolean...")
    case n: Int => println("int...")
    case Client(_, Address(_, "CABA")) =>
    case c@Client(_, Address(_, "Mendoza")) => println(s"Cliente de Mendoza: $c")
    case c: Client => println("...")
  }
