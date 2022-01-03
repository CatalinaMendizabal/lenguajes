import scala.io.StdIn
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object FuncStructs extends App:

  case class User(name: String, alias: Option[String] = None)

  def options() =
    val users = List(
      User("Juan", Some("juancito")),
      User("Pepe", None),
      User("Jose", Some("Jose88"))
    )

    val normalizedUsers = users.map { u =>
      val lower = u.alias.map(_.toLowerCase())
      u.copy(alias = lower)
    }

    for u <- normalizedUsers do
      val alias = u.alias.getOrElse("unknown")
      println(s"User: ${u.name} (alias: ${alias})")

    def readInt: Either[String, Int] =
      val in: String = StdIn.readLine("Enter an integer: ")

      try
        Right(in.toInt)
      catch
        case _: Exception => Left(in)

    def readInt2: Either[Exception, Int] =
      val in: String = StdIn.readLine("Enter an integer: ")

      try
        Right(in.toInt)
      catch
        case e: Exception => Left(e)

    // es como el either pero uno de los lados es una exception
    def readInt3: Try[Int] = Try( 
      StdIn.readLine("Enter an integer: ").toInt
    )

    val result = readInt

    result match
      case Right(n) =>
        println(s"value: ${n}")
      case Left(pattern) =>
        println(s"Invalid text: ${pattern}")

    val v: Try[Int] = readInt3

    v match {
      case Success(value) => println(s"value: $value")
      case Failure(exception) => println("error...")
    }

    v.toOption.getOrElse(-1)

    
    case class Person(id: String,
                      first: Option[String],
                      last: Option[String])

    val p = Person("a1", Some("Pepe"), Some("Juan"))
    
    val nombreCompleto: Option[String] = p.first.flatMap { fName =>
      p.last.map { lName =>
        s"$fName $lName"
      }
    }
    
    val fullName = for {
      first <- p.first
      last <- p.last
    } yield s"$first $last"
    
  options()