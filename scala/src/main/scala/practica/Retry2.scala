package practica

object RetryElement  {

  private class RetryException extends RuntimeException
  private val retryException = new RetryException

  def retryable(op: => Unit): Unit = {

    var cont = true

    while (cont) {
      try {
        cont = false
        op
      } catch {
        case _: RetryException =>
          cont = true
      }
    }
  }

  def retry(cond: Boolean): Unit = {
    if (cond) {
      throw retryException
    }
  }
}

object RetryApp extends App {
  import RetryElement._

  var i = 0

  retryable {
    i = i + 1
    println("i: " + i)
    retry(i < 10)
  }
}
