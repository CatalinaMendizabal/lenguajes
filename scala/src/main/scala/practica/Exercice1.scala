package practica

import java.io.File

import scala.io.{Source, StdIn}

object Exercice1 extends App {


  /*
  1) Implementar una función que reciba un texto (por ejemplo el contenido de un libro)
  como parámetro y devuelva como resultado una lista con las frases más populares del libro.
  Debe considerarse como una frase, cualquier secuencia de 3 palabras dentro de una oración
  (separadas por punto).
   */

  def phrases(text: String, phraseLength: Int = 3): Seq[List[String]] = {

    // Sentences are the text separated by a dot (.)
    val sentences: List[String] = text
      .toLowerCase
      .split("\\.")
      .toList

    val phrasesList = sentences.flatMap { sentence =>

      // Split each sentence word by word
      val words = sentence.split("[^a-z]")
        .filter(_.nonEmpty)
        .toList

      // Group consecutive words as phrases
      words.sliding(phraseLength)
    }

    // Sliding can return phrases shorter than specified length
    phrasesList
      .filter(_.length == phraseLength)
  }

  def topPhrases(phrases: Seq[List[String]], n: Int = 10): Seq[(String, Int)] = {

    // Map from phrases to phrase count. A phrase is represented by a List of words (strings)
    val freqMap: Map[List[String], Int] = phrases
      .groupBy(e => e)
      .map {
        case (key, value) =>
          key -> value.size
      }

    // Select top phrases (sorting them by frequency)
    val top: Seq[(List[String], Int)] = freqMap
      .toList
      .sortBy(-_._2)
      .take(n)

    // Convert List of words back to a single string
    top
      .map {
        case (key, value) =>
          key.mkString(" ") -> value
      }
  }

  // Testing the functions:

}
