package practica

import scala.concurrent.Future
import scala.io.Source
import scala.math.abs
import scala.util.{Failure, Success}

/**
Crear una estructura jerárquica con “Personas” que represente la relación
  padre-hijo. Crear un modelo de ejemplo, con una lista, donde cada elemento está
  formado por un “árbol” de “personas”.
 */
case class Person(name: String, age: Int, children: List[Person] = Nil):

  /**
   * @return Descendientes (incluyendo a la propia persona)
   */
  def descendants: List[Person] = this :: children.flatMap(_.descendants)

  /**
   * @return Personas menores y mayores (utilizar la lista de descendientes y la función partition)
   */
  def adultsAndMinors: (List[Person], List[Person]) = descendants.partition(_.age >= 18)

  /**
   * @return Personas sin hijos
   */
  def withoutChildren: List[Person] = descendants.filter(_.children.isEmpty)

  /**
   * @return Personas con hijos
   */
  def withChildren: List[Person] = descendants.filter(_.children.nonEmpty)

  /**
   * @return Hermanos mellizos (zip)
   */
  def twins = withChildren.flatMap(_.twinChildren)
  def twinChildren: List[(Person, Person)] = consecutiveChildren.filter((a, b) => a.age == b.age)
  def consecutiveChildren: List[(Person, Person)] = childrenByAge.zip(childrenByAge.tail)
  def childrenByAge: List[Person] = children.sortBy(_.age)

  /**
   * @return Hermanos con más de 4 años de diferencia de edad (zip)
   */
  def fourYearDifferenceBrothers = withChildren.flatMap(_.fourYearDifferenceChildren)
  def fourYearDifferenceChildren = consecutiveChildren.filter((a,b) => b.age - a.age > 4) // this does not work

  /**
   * @return Personas con hijos de 4 años en promedio
   */
  def fourYearAverage = withChildren.filter(_.averageChildrenAge == 4)
  def averageChildrenAge = children.foldLeft(0)(_ + _.age) / children.size

  /**
   * @param limit El limite para la edad del padre
   * @return Personas con padres mayores que una edad pasada por parámetro
   */
  def childrenWithParentOlderThan(limit: Int): List[Person] = withChildren.filter(_.age > limit).flatMap(_.children)

  /**
   * @return Todos los nietos
   */
  def allGrandChildren = withChildren.flatMap(_.grandChildren)
  def grandChildren = children.flatMap(_.children)

/**
 * Implementar una función que reciba un texto (por ejemplo el contenido de un
 * libro) como parámetro y devuelva como resultado una lista con las frases más
 * populares del libro. Debe considerarse como una frase, cualquier secuencia de 3
 * palabras dentro de una oración (separadas por punto).
 * @param text El texto a evaluar.
 * @param phraseLength La longitud de las frases deseada. (Defaults to 3)
 */
def phrases(text: String, phraseLength: Int = 3): List[List[String]] =

  // Sentences are the text separated by a dot (.)
  val sentences: List[String] = text
    .split('.')
    .toList

  val phrasesList = sentences.flatMap { sentence =>

    // Split each sentence word by word
    val words = sentence.split(' ')
      .toList

    // Group consecutive words as phrases
    words.sliding(phraseLength)
  }

  // Sliding can return phrases shorter than specified length
  phrasesList.filter(_.length == phraseLength)

def topPhrases(text: String): List[(String, Int)] =
  phrases(text).groupBy(e => e).map {
    case (phrase, phraseList) =>
      phrase.reduce(_ ++ " " ++ _) -> phraseList.size
  }.toList.sortBy(-_._2)

/**
 * Implementar una función que reciba un texto como parámetro y devuelva un
 * mapa que permita dadas dos palabras, obtener la siguiente palabra (la más
 * frecuente). El mapa puede tener la siguiente parametrización Map[List[String], String]
 * @param words Lista de palabras
 */
def mostFreqWord(words: List[String]): String = {
  val (topWord, wordCount) = words.groupBy(e => e).map {
    case (word, wordList) =>
      word -> wordList.length
  }.toList.maxBy(_._2)

  topWord
}

def nextWordMap(content: String): Map[List[String], String] = {

  val allPhrases: List[List[String]] = phrases(content)

  // Group 3-word phrases by the first two words (_.take(2))
  allPhrases
    .groupBy(_.take(2)) // all phrases have 3 words, but we take first two words to group them
    .map {
      case (phraseHead, phraseList) =>
        // phraseHead are a List of two words
        // phraseList is a list of 3-words phrases, we only need the last word
        val lastWords: List[String] = phraseList.map(_.last)

        // from all the 'lastWords', select the most frequent
        val word = mostFreqWord(lastWords)

        phraseHead -> word
    }
}

/**
 * Implementar una case class para almacenar un String localizado con
 * información sobre el/los lenguajes en los que se encuentra escrito.
 * @param location List of languages of the string
 * @param content Content of the string
 */
case class LocalizedString(location: List[String], content: String):
  /**
   * Definir una función que permita concatenar dos String localizados con el
   * operador ‘+’.
   * @param a string to add
   * @return added string
   * @example myStr + otherStr
   */
  def +(a: LocalizedString): LocalizedString = LocalizedString((location ++ a.location).distinct, content ++ a.content)

  /**
   * Definir una función ‘map’ con una función de parámetro para ser aplicada en
   * el texto y generar un nuevo String localizado.
   * @param f function to perform mapping on string content
   * @return A new LocalizedString with the mapped content
   * @example str.map(_.toUpperCase)
   */
  def map(f: String => String) = LocalizedString(location, f(content))

object LocalizedString:
  /**
   * Definir una conversión implícita que permita utilizar String regulares en donde
   * se espera un String localizado.
   */
  given stringToLocalized: Conversion[String, LocalizedString] = s => LocalizedString(List(), s)

/**
 * Extender el lenguaje scala para que soporte una estructura “retryable/retry”
 * @example
 * import Retry._
 * var i = 0
 * retryable {
 *  i = i + 1
 *  println(“i=” + i)
 *  retry (i < 10)
 * }
 */
object Retry:
  private class RetryException extends RuntimeException
  private val e = new RetryException

  def retry(cond: Boolean): Unit =
    if (cond) throw e

  def retryable(op: => Unit): Unit =
    var cont = true

    while (cont)
      try
        cont = false
        op
      catch
        case _: RetryException =>
          cont = true

import concurrent.ExecutionContext.Implicits.global
/**
 * Implementar una función que reciba dos url y compare si el contenido de ambos
 * tienen el mismo tamaño. En cuyo caso debe devolver true. Ambos URL deben ser
 * bajados concurrentemente con Futures.
 * @param url url to download
 * @return future with downloaded string
 */
def downloadAsync(url: String): Future[String] =
  Future {
    val source = Source.fromURL(url)
    val text = source.mkString
    source.close()
    println(s"Downloaded $url")
    text
  }

def compareUrlContent(url1: String, url2: String): Future[Boolean] =
  val a = downloadAsync(url1)
  val b = downloadAsync(url2)
  val l1: Future[Int] = a.map(_.length)
  val l2: Future[Int] = b.map(_.length)

  val result: Future[Boolean] = for {
    v1 <- l1
    v2 <- l2
  } yield v1 == v2

  result

def getRows(csv: String): List[List[String]] = csv.split('\n').flatMap(_.split(',')).toList.sliding(4, 4).toList

def tempSum(rows: List[List[String]]) = rows.foldLeft(0)(_ + _.last.toInt)

/**
 * Determinar cuando se produjo el cambio más brusco de temperatura entre dos
 * días consecutivos.
 */
def mostBrusqueTemperatureChange(csv: String) =

  val rows = getRows(csv)

  rows.zip(rows.tail).maxBy((a, b) => abs(a.last.toInt - b.last.toInt))

/**
 * Cuál fue el mes más caluroso ? (teniendo en cuenta la temperatura promedio)
 */
def monthAvg(csv: String) =

  val rows = getRows(csv)

  rows.groupBy(_(1)).map { // tmb _.tail.head
    case (month, rows) =>
      month -> tempSum(rows) / rows.size
  }.toList.maxBy(_._2)

/**
 * Cuáles fueron los 3 días consecutivos más calurosos?
 */
def threeMostHotConsecutive(csv: String) =

  val rows = getRows(csv)

  rows.sliding(3).maxBy(tempSum(_)/3)

/**
 * Extender el lenguaje Scala con una construcción "logIfSlow" para imprimir por
 * consola un mensaje de warning si la ejecución de un bloque de código se extiende
 * por más de 1 segundo.
 */
object LogIfSlow:
  private class SlowException extends RuntimeException
  private val e = new SlowException

  def log(cond: Boolean): Unit =
    if(cond) throw e

  def logIfSlow(op: => Unit): Unit =
    var now = System.currentTimeMillis()

    try
      op
      log(System.currentTimeMillis() - now > 1000)
    catch
      case _: SlowException =>
        println("Slow code detected")

  def logIfSlow2(op: => Unit): Unit =
    val execution = Future {
      op
    }
    Thread.sleep(1000)
    if(!execution.isCompleted) println("Slow code detected")

sealed abstract class Tree
case class Branch(left: Tree, right:Tree) extends Tree
case class Leaf(x: Int) extends Tree

def sumLeafValue(tree: Tree): Int =
  tree match {
    case Branch(l, r) => sumLeafValue(l) + sumLeafValue(r)
    case Leaf(x) => x
  }

def countLeaves(tree: Tree): Int =
  tree match {
    case Branch(l, r) => countLeaves(l) + countLeaves(r)
    case Leaf(x) => 1
  }

/**
 * Una función que calcule el valor promedio de los datos en las hojas (usar
 * pattern matching).
 */
def avgLeafValue(tree: Tree): Int =
  sumLeafValue(tree) / countLeaves(tree)

/**
 * Una función que devuelva una lista con todas las hojas
 */
def leaves(tree: Tree): List[Leaf] =
  tree match {
    case Branch(l, r) => leaves(l) ++ leaves(r)
    case Leaf(x) => List(Leaf(x))
  }

/*
1) Dado el resultado de ventas de una empresa, almacenado en un archivo de texto
separado por comas, con el siguiente formato:
[Provincia, Distrito, Producto, Cantidad], por ejemplo
Buenos Aires, Pilar, A, 2300
Buenos Aires, Pilar, B, 2800
Buenos Aires, Pilar, C, 1700
Buenos Aires, San Isidro, A, 2400
Buenos Aires, San Isidro, B, 3100
Buenos Aires, San Isidro, C, 1500
...
Implementar una ‘case class’ para almacenar cada reporte de ventas (fila), una función que
cargue los valores desde un archivo y funciones (puras, sin variables mutables) que
devuelvan
a. la cantidad total de productos total vendidos en
b. el producto más vendido (A, B o C)
c. la provincia en la cual obtuvo mejores ventas un producto pasado como parámetro
*/

case class Venta(provincia: String, distrito: String, producto: String, cantidad: Int)

def getFile(path: String): List[Venta] =
  val file = Source.fromFile(path).mkString
  val sales = file.split('\n').flatMap(_.split(',')).map(_.trim).toList.sliding(4, 4).filter(_.length == 4).toList
  sales.map {
    case List(prov, dist, prod, cant) =>
      Venta(prov, dist, prod, cant.toInt)
  }

def total(sales: List[Venta]): Int = sales.map(_.cantidad).sum

def masVendido(sales: List[Venta]): String =
  sales.groupBy(_.producto).map {
    case (prod, list) =>
      prod -> total(list)
  }.toList.maxBy(_._2)._1

def provMejor(prod: String, sales: List[Venta]): String =
  sales.filter(_.producto == prod).groupBy(_.provincia).map {
    case (prov, ventas) =>
      prov -> total(ventas)
  }.toList.maxBy(_._2)._1

@main def main = {

  val file = getFile("sales")
  println(masVendido(file))
  println(provMejor("B", file))

  //  import scala.language.implicitConversions
  //  import LocalizedString.given
  //
  //  val a = LocalizedString(List("es"), "Hola")
  //  val b = LocalizedString(List("en"), "World")
  //
  //  println(a + b)
  //
  //  println(a.map(_.toUpperCase))
  //
  //  println(a + " " + b)
  //
  //  import Retry._
  //
  //  var i = 0
  //  retryable {
  //    i = i + 1
  //    println("i=" + i)
  //    retry (i < 10)
  //  }

  //  val result = compareUrlContent("https://scala-lang.org", "https://scala-lang.org")
  //
  //  result.onComplete {
  //    case Success(v) => println(v)
  //    case Failure(e) => println("error")
  //  }
  //
  //  println("Waiting...")
  //  System.in.read()

  //  val csv = "2015,03,05,20\n2015,03,06,28\n2015,03,07,31\n2015,03,08,41\n2015,03,09,40\n" +
  //    "2015,04,05,30\n2015,04,06,31\n2015,04,07,29\n2015,04,08,35\n2015,04,09,40"
  //
  //  println(mostBrusqueTemperatureChange(csv))
  //  println(monthAvg(csv))
  //  println(threeMostHotConsecutive(csv))

  //  import LogIfSlow._
  //
  //  println("1st")
  //  logIfSlow {
  //    1 + 1
  //    println("very fast")
  //  }
  //  println("2nd")
  //  logIfSlow {
  //    Thread.sleep(2000)
  //    1 + 1
  //    println("that was slow!")
  //  }

  //  println(sumLeafValue(Branch(Branch(Leaf(5), Leaf(3)), Leaf(6))))
  //  println(avgLeafValue(Branch(Branch(Leaf(5), Leaf(3)), Leaf(6))))
  //  println(leaves(Branch(Branch(Leaf(5), Leaf(3)), Leaf(6))))
}
