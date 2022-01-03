trait Friendly:
  def name: String

  def sayHello(): Unit =
    println(s"Hello $name")

trait Gamer:
  def play(): Unit =
    println(s"Playing...")

trait Children:
  private var children: List[Person] = Nil

  def addChild(child: Person) = children = child :: children
  def getChildren: List[Person] = children

case class Person(name: String, age: Int) extends Friendly, Gamer

case class Robot(name: String) extends Friendly, Gamer

object Traits extends App:

  def sayHello(p: Friendly): Unit =
    p.sayHello()

  def playWith(g: Gamer): Unit =
    p.play()

  //intersection Friendly and Gamer
  def playAndTalk(p: Friendly & Gamer): Unit =
    p.play()
    p.sayHello()

  val p = Person("Juan", 69)
  val n = p.age

  p.sayHello()

  // el with se llama mixing
  val dad: Person & Children = new Person("Juan", 80) with Children

  dad.addChild(p)

  playAndTalk(p)
  playAndTalk(Robot("R2-D2"))

