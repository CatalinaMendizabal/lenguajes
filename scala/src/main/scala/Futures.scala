import scala.io.Source
import scala.util.{Failure, Success}

object Futures extends App:
  import scala.concurrent.Future
  import concurrent.ExecutionContext.Implicits.global

  def downloadSync(url: String): String =
    val source = Source.fromURL(url)
    val text = source.mkString
    source.close()
    println(s"Downloaded $url")
    text

  // Devuelve un Future[String], no un string
  def downloadAsync(url: String): Future[String] =
    Future {
      val source = Source.fromURL(url)
      val text = source.mkString
      source.close()
      println(s"Downloaded $url")
      text
    }

  val c1 = downloadAsync("https://scala-lang.org")
  val c2 = downloadAsync("https://scala-android.org")
  val c3 = downloadAsync("https://wikipedia.org")


  val l1: Future[Int] = c1.map(_.length)
  val l2: Future[Int] = c2.map(_.length)
  val l3: Future[Int] = c3.map(_.length)

//  val result = l1.flatMap {v1 =>
//    l2.flatMap {v2 =>
//      l3.map {v3 =>
//        v1 + v2 + v3
//      }
//    }
//  }

  val result: Future[Int] = for {
    v1 <- l1
    v2 <- l2
    v3 <- l3
  } yield v1 + v2 + v3

  result.onComplete {
    case Success(value) => println(s"Total length: $value")
    case Failure(ex) => println(s"Failed: $ex")
  }


  println("Waiting...")
  System.in.read()
//  c1.length + c2.length + c3.length

