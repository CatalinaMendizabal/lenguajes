import scala.io.Source

object Collections extends App:

  val list = List (1,2,3,4,5,6)

  list.foreach((e:Int) => println(e))
  list foreach println // es lo mismo q la de arriba buenardo

  val count = list.count(_ > 2)
  list.exists(_ > 3)
  list.filter(_ != 2)
  val cond = list.forall(_ > 0)

  val h = list.head
  val hOption: Option[Int] = list.headOption
  println(s"head: ${hOption.getOrElse("-")}")

  list.init
  list.last
  list.length > 40
  list.lengthCompare(40) // es mas eficiente q hacer lo de arriba

  println(list.mkString("< ", ", ", ">" ))

  val temps = List(18, 19, 18, 21, 24, 25, 27, 26)

  println(temps.sliding(3).map(_.sum/3.0).max)

  val l = List("23", "ddd")
  println(l.min)
  // l.sum no anda

  temps.grouped(3).foreach(println)

  val firsts = temps.take(3)

  println(temps.takeWhile(_ <= 25))

  temps.takeRight(3)

  temps.drop(3)
  temps.dropRight(2)
  temps.dropWhile(_ < 24)

  temps.reduce(_ + _) // temps.sum; Reduce con lista vacia falla

  val numbers = List(1,2,3,4,5)
  val strings = List("uno", "dos", "tres")

  val result = numbers.zip(strings)
  result.foreach(println)

  val delta = temps.zip(temps.tail).map {
    case (a,b) =>
      b - a
  }

  delta foreach println
  println("max delta: " + delta.map(_.abs).max)

  // fold

  numbers.foldLeft(0)(_ + _) // lo mismo que numbers.sum

  // permutations and combinations
  numbers.permutations.foreach(println)
  numbers.combinations(3).foreach(println)

  val temps2 = List(18, 19, 18, 21, 24, 25, 27, 26)

  // partition
  val (altas, bajas) = temps2.partition(_ > 24)

  println(altas)
  println(bajas)

  val words = List("hello world", "chau mundo").map(_.split(" ")).flatten
  val words2 = List("hello world", "chau mundo").flatMap(_.split(" "))
  println(words)

  val words3 = List("hola", "mundo", "chau", "demo", "ok", "scala", "lang", "help", "lenguaje")
  val wordsByLength = words3.groupBy(_.length)

  wordsByLength.foreach(println)

  words3.groupBy(_.head).foreach(println)
  words3.groupBy(_.head).map {
    case (firstLetter, wordsList) =>
        firstLetter -> wordsList.map(_.length)
  }.foreach(println)

  words3.groupMap(_.head)(_.length).foreach(println) // lo de arriba se puede hacer asi también

  words3.groupMapReduce(_.head)(_.length)(_ + _).foreach(println)


  // find
  val first: Option[String] = words3.find(_.length == 4) // busca el primero es un Option

  // diff
  List(1,2,3,4,5) diff List(1,2,3)

  //intersection
  List(1,2,3,4,5) intersect  List(2,3)

  //distinct
  List(1,2,3,1,2,4,5,1).distinct

  words3.min
  words3.max
  words3.maxBy(_.length)
  words3.minBy(_.length)

  def wordFreq(): Unit = {

    val source = Source.fromURL("https://github.com/TomasBruno1/RetroMove#readme")

    val lines = source.getLines().toList

    val words = lines
      .map(_.toLowerCase)
      .flatMap(_.split("[^a-z]"))
      .filter(_.nonEmpty)

    val freq = words
      .groupBy(identity)
      .map {
        case (k, v) => k -> v.length
      }

    val top = freq.toList.sortBy(- _._2).take(50) // ordena por el segundo elemento, como es entero le pone el - para q sea decreciente
    top.foreach(println)
  }

  wordFreq()


