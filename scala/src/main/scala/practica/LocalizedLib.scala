package practica


/*
 * 3)  Implementar una case class para almacenar un String localizado con información sobre el/los
 *  lenguajes en los que se encuentra escrito.
 *
 * Por ejemplo:
 *
 * LocalizedString(List(“es”), “Hola Mundo”)
 * LocalizedString(List(“en”), “Hello World”)
 *
 *
 * Definir una función que permita concatenar dos String localizados con el operador ‘+’.
 * Ej: myStr + otherStr
 *
 * Definir una función ‘map’ con una función de parámetro para ser aplicada en el texto y generar
 * un nuevo String localizado.
 *
 * Ej: val newStr = str.map(_.toUpperCase)
 *
 * Definir una conversión implícita que permita utilizar String regulares en donde se espera un String localizado. Ej:
 * val result = myLocalizedString + “ “ + otherLocalizedString
 */

object LocalizedLib {

  implicit def localizedToString(loc: LocalizedString): String = {
    loc.text
  }

  case class LocalizedString(langs: Seq[String], text: String) {

    def +(other: LocalizedString): LocalizedString = {
      copy(
        langs = (langs ++ other.langs).distinct,
        text = text + other.text
      )
    }

    def map(f: String => String): LocalizedString = {
      copy(text = f(text))
    }
  }

}

object LibApp extends App {
  import LocalizedLib._

  val loc1 = LocalizedString(List("en"), "Hello world.")
  val loc2 = LocalizedString(List("es"), "Hola Mundo ")

  val loc3 = loc2 + loc1

  val string: String = loc3
  println(loc3)
  println(string)
}