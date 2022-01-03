import OpaqueTypeAlias.Distance

object TypeAlias :

  // type alias
  type FreqMap = Map[String, Int]

  def someFunc1(freq: Map[String, Int]): Unit = {}

  def someFunc2(freq: FreqMap): Unit = {}

object OpaqueTypeAlias:
  opaque type Distance = Double
  opaque type Rate = Double

  object Distance:
    def apply(d: Double): Distance = d

  extension(d: Distance)
    def toDouble: Double = d
    def +(o: Distance): Distance = d + o

  object Rate:
    def apply(d: Double): Rate = d

//  extension(d: Rate)
//    def toDouble: Double = d
//    def +(o: Rate): Rate = d + o

object TypeDemo extends App:
  
  import TypeAlias.*
  import OpaqueTypeAlias.*
  val m: FreqMap = Map("" -> 1)

  TypeAlias.someFunc1(Map("" -> 1))
  TypeAlias.someFunc2(Map("" -> 1))

  // Double y Distance no son intercambiables
  val d: Distance = Distance(3.4)
  val r: Rate = Rate(0.4)

  def myFunct(r: Rate, d: Distance): Unit = {}

  myFunct(r, d)
  // myFunct(d, r) no te deja por ser alias opaco







