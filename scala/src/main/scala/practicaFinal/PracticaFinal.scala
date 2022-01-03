package practicaFinal

import scala.concurrent.Future
import scala.io.Source
import scala.math.abs
import scala.util.{Failure, Success}

case class Person(name: String, age: Int, children: List[Person] = Nil):

  // a) Descendientes (incluyendo a la propia persona)
  def descendants: List[Person] = this :: children.flatMap(_.descendants)

  // b) Personas menores y mayores (utilizar la lista de descendientes y la función partition)
  def adultsAndMinors: (List[Person], List[Person]) = descendants.partition(_.age > 18)

  // c) Personas sin hijos
  def noChildren = descendants.filter(_.children.isEmpty)

  def withChildren = descendants.filter(_.children.nonEmpty)

  // d) Hermanos mellizos (zip)
  def orderByAge: List[Person] = children.sortBy(_.age)

  def consecutiveChildren: List[(Person, Person)] = orderByAge.zip(orderByAge.tail)

  def sameAge: List[(Person, Person)] = consecutiveChildren.filter { case (a, b) => a.age == b.age }

  def twinChildren = withChildren.flatMap(_.sameAge)

  // e) Hermanos con más de 4 años de diferencia de edad (zip)
  def diferenceFour: List[(Person, Person)] = consecutiveChildren.filter { case (a, b) => a.age - b.age > 4 }

  def fourYearDif: List[(Person, Person)] = withChildren.flatMap(_.diferenceFour)

  // f) Personas con hijos de 4 años en promedio
  def averageChildrenAge: Double = children.map(_.age).sum / children.size

  //   def averageChildrenAge = children.foldLeft(0)(_ + _.age) / children.size
  def fourYearAvg: List[Person] = withChildren.filter(_.averageChildrenAge == 4)

  // g) Personas con padres mayores que una edad pasada por parámetro
  def childrenWithParentOlderThan(limit: Int): List[Person] = withChildren.filter(_.age > limit).flatMap(_.children)

  // h) Todos los nietos
  def grandChildren: List[Person] = withChildren.flatMap(_.children.flatMap(_.children))


/*
  2) Implementar una función que reciba un texto (por ejemplo el contenido de un
  libro) como parámetro y devuelva como resultado una lista con las frases más
  populares del libro. Debe considerarse como una frase, cualquier secuencia de 3
  palabras dentro de una oración (separadas por punto).
*/

def phrases(text: String): List[List[String]] =
  val sentences = text.split('.').toList
  val phraseList = sentences.flatMap { phrases =>phrases.split(' ').toList.sliding(3) }
  // porque el sliding puede devolver menos de 3 tmb
  phraseList.filter(_.length == 3)

def topPhrases(text: String) =
  phrases(text).groupBy(e => e).map {
    case (phrase, phraseList) =>
      phrase.reduce(_ ++ " " ++ _) -> phraseList.size
  }.toList.sortBy(-_._2)

/*
  3) Implementar una función que reciba un texto como parámetro y devuelva un
  mapa que permita dadas dos palabras, obtener la siguiente palabra (la más
  frecuente). El mapa puede tener la siguiente parametrización Map[(String, String),
  String]
*/

def nextWord(texto: String) =
  val frases: List[List[String]] = phrases(texto)

  frases.groupBy(_.take(2)).map {
    case (head, list) =>
      val ultimas = list.map(_.last)
      val palabra = masFrecuente(ultimas)
  }

def masFrecuente(value: List[String]) =
  val (top, cant) = value.groupBy(e => e).map {
    case (palabra, lista) => palabra -> lista.length
  }.toList.maxBy(-_._2)
  top

/*
  4) Implementar una case class para almacenar un String localizado con
  información sobre el/los lenguajes en los que se encuentra escrito.
*/

case class LocalizedString(location: List[String], content: String):

  /*
    a) Definir una función que permita concatenar dos String localizados con el
    operador ‘+’. Ej: myStr + otherStr
  */
  def +(a: LocalizedString) = LocalizedString((location ++ a.location).distinct, content ++ a.content)

  /*
   b) Definir una función ‘map’ con una función de parámetro para ser aplicada en
   el texto y generar un nuevo String localizado.
   Ej: val newStr = str.map(_.toUpperCase)
  */
  def map(f: String => String) = LocalizedString(location, f(content))

/*
  c) Definir una conversión implícita que permita utilizar String regulares en donde
  se espera un String localizado. Ej: val result = myLocalizedString + “ “ + otherLocalizedString
*/

object LocalizedString:
  given stringToLocalized: Conversion[String, LocalizedString] = s => LocalizedString(List(), s)

/*
   5) Extender el lenguaje scala para que soporte una estructura “retryable/retry” que
  permita expresar la siguiente construcción:
    import Retry._
      var i = 0
      retryable {
        i = i + 1
        println(“i=” + i)
        retry (i < 10)
    } --> esta en practica.scala porque me dio paja
*/

/*
  ejercico 7 guia
   * Implementar una función que reciba dos url y compare si el contenido de ambos
   * tienen el mismo tamaño. En cuyo caso debe devolver true. Ambos URL deben ser
   * bajados concurrentemente con Futures
*/

import concurrent.ExecutionContext.Implicits.global

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


/*
  Ejercicio 8: Dado un archivo CSV (texto separado por comas) que contiene todas las
  temperaturas del año. Por ejemplo con los campos: año, mes, día, temperatura
*/
def getRows(csv: String): List[List[String]] = csv.split('\n').flatMap(_.split(',')).toList.sliding(4, 4).toList

def tempSum(rows: List[List[String]]) = rows.foldLeft(0)(_ + _.last.toInt)

/*
  Determinar cuando se produjo el cambio más brusco de temperatura entre dos
  días consecutivos.
*/

def mostBrusqueTemperatureChange(csv: String) =

  val rows = getRows(csv)

  rows.zip(rows.tail).maxBy((a, b) => abs(a.last.toInt - b.last.toInt))

/* b) Cuál fue el mes más caluroso ? (teniendo en cuenta la temperatura promedio)*/

def monthAvg(csv: String) =

  val rows = getRows(csv)

  rows.groupBy(_ (1)).map { // tmb _.tail.head
    case (month, rows) =>
      month -> tempSum(rows) / rows.size
  }.toList.maxBy(_._2)

/* c) Cuáles fueron los 3 días consecutivos más calurosos ? */

def threeMostHotConsecutive(csv: String) =

  val rows = getRows(csv)

  rows.sliding(3).maxBy(tempSum(_) / 3)

/**
 * Extender el lenguaje Scala con una construcción "logIfSlow" para imprimir por
 * consola un mensaje de warning si la ejecución de un bloque de código se extiende
 * por más de 1 segundo.
 */

object LogIfSlow:
  private class SlowException extends RuntimeException

  private val e = new SlowException

  def log(cond: Boolean): Unit =
    if (cond) throw e

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
    if (!execution.isCompleted) println("Slow code detected")

/* Ejercicio 11 */

sealed abstract class Tree

case class Branch(left: Tree, right: Tree) extends Tree

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
1) Crear una estructura jerárquica con “Personas” que represente la relación
padre-hijo. Crear un modelo de ejemplo, con una lista, donde cada elemento está
formado por un “árbol” de “personas”.
Ejemplo de clase “Person”:
case class Person(name: String, age: Int, children: List[Person] = Nil)
Implementar funciones que permitan obtener:
a) Descendientes (incluyendo a la propia persona)
b) Personas menores y mayores (utilizar la lista de descendientes y la función
partition)
c) Personas sin hijos
d) Hermanos mellizos (zip)
e) Hermanos con más de 4 años de diferencia de edad (zip)
f) Personas con hijos de 4 años en promedio
g) Personas con padres mayores que una edad pasada por parámetro
h) Todos los nietos
*/

case class Persona2(nombre: String, edad: Int, hijos: List[Persona2] = Nil):

  def descendientes: List[Persona2] = this :: hijos.flatMap(_.descendientes)

  def mayoresYMenores: (List[Persona2], List[Persona2]) = descendientes.partition(_.edad >= 18)

  def sinHijos = descendientes.filter(_.hijos.isEmpty)

  def conHijos = descendientes.filter(_.hijos.nonEmpty)

  def hijosConsecutivos: List[Persona2] = hijos.sortBy(_.edad)
  def dosHijos: List[(Persona2, Persona2)] = hijosConsecutivos.zip(hijosConsecutivos.tail)
  def hermanosMellizos: List[(Persona2, Persona2)] = dosHijos.filter{ case (a, b) => a.edad == b.edad }
  def mellis: List[(Persona2, Persona2)] = conHijos.flatMap(_.hermanosMellizos)

  def masDe4: List[(Persona2, Persona2)] = dosHijos.filter { case (a,b) => (a.edad - b.edad) > 4}

  def promedioEdad: Int = hijos.foldLeft(0)(_ + _.edad) / hijos.length
  def promedio4 = conHijos.filter(_.promedioEdad == 4)

  def mayoresDe (edad: Int): List[Persona2] = conHijos.filter(_.edad > edad).flatMap(_.hijos)

  def nietos = conHijos.flatMap(_.hijos.flatMap(_.hijos))

/*
  2) Implementar una función que reciba un texto (por ejemplo el contenido de un
  libro) como parámetro y devuelva como resultado una lista con las frases más
  populares del libro. Debe considerarse como una frase, cualquier secuencia de 3
  palabras dentro de una oración (separadas por punto).
*/

def frasesTexto(texto: String): List[List[String]] =
  val frases = texto.split('.').toList // separo por frases
  val palabras = frases.flatMap { frase => frase.split(' ').toList.sliding(3)} // separo frase por 3 palabras

  palabras.filter(_.length == 3) // filtro si tiene menos de 3

def topfrases(texto: String) =
  frasesTexto(texto).groupBy(e => e).map {
    case (frase, frases) =>
      frase.reduce(_ ++ " " ++ _) -> frases.size
  }.toList.sortBy(-_._2)

/*
3) Implementar una función que reciba un texto como parámetro y devuelva un
mapa que permita dadas dos palabras, obtener la siguiente palabra (la más
frecuente). El mapa puede tener la siguiente parametrización Map[(String, String),
String] */

def masPopular(value: List[String]) =
  val (palabraTop, cantidad) = value.groupBy(e => e).map {
    case (palabra, palabras) =>
      palabra -> palabras.length
  }.toList.maxBy(_._2)

def funA(texto: String) =
  val frases = frasesTexto(texto)
  frases.groupBy(_.take(2)).map {
    case (primerFrase, listaFrases) =>
      val ultima = listaFrases.map(_.last)
      val palabra = masPopular(ultima)

      primerFrase -> palabra
  }

case class StringLocalizador(pais: List[String], contenido: String):

  def +(a: StringLocalizador) = StringLocalizador((this.pais ++ a.pais).distinct, this.contenido ++ a.contenido)

  def map2(f: String => String) = StringLocalizador(pais, f(contenido))

object StringLocalizador:
  given stringALocalizar: Conversion[String, StringLocalizador] = s => StringLocalizador(List(), s)


def descargarUrl(url: String): Future[String] =
  Future {
    val source = Source.fromURL(url)
    val texto = source.mkString
    source.close()
    texto
  }

def compararUrl(url1: String, url2: String): Future[Boolean] =
  val u1 = descargarUrl(url1)
  val u2 = descargarUrl(url2)
  val tam1: Future[Int] = u1.map(_.length)
  val tam2: Future[Int] = u2.map(_.length)

  val resultado: Future[Boolean] = for {
    v1 <- tam1
    v2 <- tam2
  } yield v1 == v2

  resultado

/* 8) Dado un archivo CSV (texto separado por comas) que contiene todas las
temperaturas del año. Por ejemplo con los campos: año, mes, día, temperatura
a) Determinar cuando se produjo el cambio más brusco de temperatura entre dos
días consecutivos.
b) Cuál fue el mes más caluroso ? (teniendo en cuenta la temperatura promedio)
c) Cuáles fueron los 3 días consecutivos más calurosos ?
*/

/*
9) Extender el lenguaje Scala con una construcción "logIfSlow" para imprimir por
consola un mensaje de warning si la ejecución de un bloque de código se extiende
por más de 1 segundo. Por ejemplo:
logIfSlow {
<<código>>
}
Si el <<código>> tarda más de 1 segundo debe imprimir:
"Slow code detected"
*/

/*
11) Dada las siguientes case classes
sealed abstract class Tree
case class Branch(left: Tree, right:Tree) extends Tree
case class Leaf(x: Int) extends Tree
Implementar las siguientes funciones:
a) Una función que calcule el valor promedio de los datos en las hojas (usar
pattern matching).
b) Una función que devuelva una lista con todas las hojas
*/

sealed abstract class Tree2
case class Branch2(left: Tree2, right:Tree2) extends Tree2
case class Leaf2(x: Int) extends Tree2

def sumarHojas(tree: Tree2): Int =
  tree match {
    case Branch2(l, r) => sumarHojas(l) + sumarHojas(r)
    case Leaf2(x) => x
  }

def cantidadHojas(tree: Tree2): Int =
  tree match {
    case Branch2(l, r) => cantidadHojas(l) + cantidadHojas(r)
    case Leaf2(x) => 1
  }

def promedioHojas(tree: Tree2): Int =
  sumarHojas(tree) / cantidadHojas(tree)

def hojasA(arbol: Tree2):  List[Leaf2] =
  arbol match {
    case Branch2(left, right) => hojasA(left) ++ hojasA(right)
    case Leaf2(x) => List(Leaf2(x))
  }


// EJERCICIO FINAL MAU

/*

val list = List(
Tweet(
author = "rockthejvm",
text = "#Scala is not just a better #java",
date = "2020-10-01”,
country = "us"
),
Tweet(
author = "lightbend",
text = "What are the major improvements that #Java and
#Scala architects and developers will enjoy in #Akka 2.6?",
date = "2020-11-01”,
country = "it"
),
Tweet(
author = "47deg",
text = "We're extremely grateful to be involved with a
great community group in helping shape #Scala3 and providing
some engineering resources to move this forward.",
date = "2020-11-15”,
country = "uk"
)
)

a) Implementar una función que devuelva un mapa de tipo Map[Int, List[String]] en
donde la clave es el año y el valor es una lista con los 5 hashtags más usados.
b) Implementar una función que devuelva la frase de 3 palabras más popular de twitter
*/

case class Tweet(autor: String, texto: String, fecha: String, pais: String)

def hashtags(tw: List[Tweet]): Map[Int, List[String]] =
  val resultado: List[(String, List[Tweet])] = tw.groupBy(t => t.fecha.split('-').head).toList // agarro los años de los twwets ordenados por fecha
  resultado.map {
    case (year, tweets) =>
      val yearHashtags: List[String] = tweets.flatMap(t => t.texto.split(' ').filter(s => s.head == '#')).toList
      val topFive: List[String] = yearHashtags.groupBy(e => e).map {
        case (hashtag, hashtags) =>
          hashtag -> hashtags.length
      }.toList.sortBy((a,b) => -b).take(5).map((a, b) => a)
      year.toInt -> topFive
  }.toMap

def topFrases(tw: List[Tweet]) =
  val frases = tw.flatMap(e => e.texto.split(' ').toList.sliding(3).toList) // agarro de a 3
  frases.filter(_.length == 3).groupBy(e => e).map {
    case (frase, frases) => frase -> frases.length
  }.toList.sortBy((a, b) => -b).head._1.reduce(_ + " " + _)

/*
5. Implementar en Scala una función que reciba una lista de libros: case class Book (id: String,
authors: List[String], title: String) y devuelva un mapa del tipo Map[String, List[String]] donde la
clave es el autor y el valor la lista de los títulos de los libros que haya escrito
*/

case class Book (id: String, authors: List[String], title: String)

def authorMap(books: List[Book]) =
  books.flatMap { b =>
    b.authors.map { a =>
      a -> b.title}
    }.groupBy(_._1).map {
    case (author, list) =>
      author -> list.map(e => e._2)
  }

// Recu Nacho

/*
Dado el resultado de ventas de una empresa, almacenado en un archivo de texto
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
a. la cantidad total de productos vendidos
b. el producto más vendido (A, B o C)
c. la provincia en la cual obtuvo mejores ventas un producto pasado como parámetro
*/

case class Reporte(provincia: String, distrito: String, producto: String, cantidad: Int)

def cantidadTotalVendido(reportes: List[Reporte]) =
  reportes.foldLeft(0)(_ + _.cantidad)

def bestSeller(reportes: List[Reporte]) =
  val (prod, cant) = reportes.groupBy(e => e.producto).map {
    case (producto, lista) =>
      producto -> lista.map(_.cantidad).sum
  }.toList.maxBy(_._2)
  s"El producto $prod es el mas vendido con $cant vendido"

def mejoresVentas(prod: String, reportes: List[Reporte]) =
  val reportesSelec = reportes.filter(_.producto == prod)
  val (p, c) = reportesSelec.groupBy(e => e.provincia).map {
    case (prov, l) =>
      prov -> l.map(_.cantidad).sum
  }.toList.maxBy(_._2)
  p



@main def testing =
  println(mejoresVentas("A", List(
    Reporte("Bs as", "tigre", "A", 100),
    Reporte("Bs as", "tigre", "B", 40),
    Reporte("Bs as", "tigre", "B", 60),
    Reporte("Bs as", "tigre", "B", 320),
    Reporte("Bs as", "tigre", "A", 1000),
    Reporte("Cahco", "tigre", "A", 7000),
  )))

/*
  println(authorMap(List(
    Book("1", List("Brts", "Juan"), "Azala"),
    Book("1", List("Cata", "Juan"), "Literal"),
    Book("1", List("Brts", "Cata"), "Crank"),
    Book("1", List("Brts"), "Zahar"),
    Book("1", List("Brts", "Locu"), "Phite"),
    Book("1", List("Locu", "Cata", "Brts"), "..."),
    Book("1", List("Cata"), "Anashe")
  )))
*/
