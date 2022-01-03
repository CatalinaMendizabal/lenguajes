case class Book(title: String, author: String, categories: List[String])

object Jsons extends App:
  import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
  val book = Book (
    author = "Martin Odersky",
    title = "Programming in Scala",
    categories = List("programming", "scala")
  )
  val json = book.asJson.spaces4
  println(json)

  println(List(1,2,3).asJson)

  val newBook : Either[Error, Book] = decode[Book](json)
  newBook match {
    case Right(b) => println("book: " + b)
    case Left(error) => println("error...")
  }