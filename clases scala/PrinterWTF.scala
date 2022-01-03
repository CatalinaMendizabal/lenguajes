type Image = Array[Byte]

class Printer:
  def print(img: Image): Unit = {}
  def status: String = ""

class Scanner:
  def scan: Option[Image] = None
  def status: String = ""

class Copier:
  private val p = new Printer
  private val s = new Scanner

  export s.scan
  export p.{status => _, *} // no quiero status

  def status: String = p.status + s.status

object Export extends App:
  val c = new Copier

  c.scan
  c.status
