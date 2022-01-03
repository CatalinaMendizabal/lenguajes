package practica

import scala.io.StdIn

/*
 2) Implementar una función que reciba un texto como parámetro y devuelva un mapa que permita dadas
    dos palabras, obtener la siguiente palabra (la más frecuente). El mapa puede tener la siguiente
    parametrización Map[(String, String), String]

 */

object Exercice2 extends App {

  import Exercice1._

  /*
   Returns the most frequent word from a non empty list of words
   */
  def mostFreqWord(words: Seq[String]): String = {
    val (topWord, wordCount) = words.groupBy(e => e).map {
      case (word, wordList) =>
        word -> wordList.length
    }.toList.maxBy(_._2)

    topWord
  }

  def nextWordMap(content: String): Map[List[String], String] = 

    val allPhrases: Seq[List[String]] = phrases(content)

    // Group 3-word phrases by the first two words (_.take(2))
    allPhrases
      .groupBy(_.take(2)) // all phrases have 3 words, but we take first two words to group them
      .map {
        case (phraseHead, phraseList) =>
          // phraseHead are a List of two words
          // phraseList is a list of 3-words phrases, we only need the last word
          val lastWords: Seq[String] = phraseList.map(_.last)

          // from all the 'lastWords', select the most frequent
          val word = mostFreqWord(lastWords)

          phraseHead -> word
      }
  
}

