object DSL extends App:

  import Break.*

  breakable {
    for i <- 1 to 100 do
      println(i)

      if (i == 10)
        break

    end for
  }

object Break:
  private class BreakException extends RuntimeException
  private val e = new BreakException

  def break: Unit =
    throw e

  def breakable(p: => Unit): Unit = // no llegamos a copiar
    0


