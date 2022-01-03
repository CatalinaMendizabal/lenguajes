## 07/10/2021
## Scala

### Introducción
Se creó en el 2004
Mezcla programación funcional con orientada a objetos

Lo que uno hace en Java puede pasarlo a Scala. 
Es uno de los lenguajes más pagos.

Tipo estático.
El código generado puede correr en browser también (javascript)

Inferencia de tipos -> si a una variable le asignamos un valor, el compilador determina su tipo por nosotros.
val -> inmutable
var -> mutable

__Lista__
```scala 
List(1,2,3)
val list: List[Int] = List(1,2,3)
```

__Métodos de lista__
```scala 
val list: List[Int] = List(1,2,3)


list.foreach((e:Int) => println(e))
list.foreach(e => println(e))
list.foreach(println(_))
list.foreach(println)
list foreach println
list foreach { e => 
        println(e)
}

for (e  <- list) { println(e) }
for e  <- list do println(e)

for e <- list do println(double(e)) 

List(1, "hola", true) //no se preserva el tipo de cada uno

list.map(double)
list.map(_ * 2)

0 :: list
list.reverse

list ::: list

list.count( _ > 2 )

list.exists( _ == 2)

list.filter (_ >= 2)

list.forall(_ > 3)

list.head

list.tail

list.init

list.last

list.length

list.mkString("|")
list.mkString(", ")

val List(a,b,c) = list

val (a,b) = (1, "hola")
```

__Métodos__
```scala 
def double (x:Int):Int = x * 2

@main def basics(n:Int) = {
    val list = List(1,2,3)
    val newList = list.map(double)
    newList.foreach(println)
}

object MyApp extends App {
    val list = List(1,2,3)
    val newList = list.map(double)
    newList.foreach(println)
}

def factorial (n:Int):Int = n match {
    case 0 => 1
    case x if x > 0 => factorial(n-1) * n
    
    println(factorial(400))
}

```

__Sets__
```scala 
val set = Set("a", "b", "c")
```

__Métodos de sets__
```scala 
val set = Set("a", "b", "c")

set.foreach(println)

```

__Tuplas__
```scala 
(1, "hola", true) //se preserva el tipo de los elementos
1 -> "hola"
(1, "hola")
```

__Map__
```scala 
Map("hola" -> "hello", "mundo" -> "world")   //cada uno es una tupla
```

__Métodos de mapa__
```scala 
val map = Map("hola" -> "hello", "mundo" -> "world")
map.map((a,b) => (b,a))

```

__Clases__
```scala 
class Point(_x: Int, _y: Int) {
    
    val x: Int = _x
    val y: Int = _y
    
    override def toString: String = { s"Point($x, $y)")
}
// el object es como un singleton, se instancia solo y una vez nada más
object PointApp extends App {
    val p = new Point( _x = 3, _y= 4)
    println(p)
    
    
//inmutable  
case class Point(x: Int, y: Int)

object PointApp extends App {
    val p = Point(3, 4)
    println(p)
}

// objeto acompañante de la clase PointV1
object PointV1 {
    val ZERO = new PointV1(_x = 0, _y =0)
    
    def create(x: Int, y: Int) =
        if (x == 0 && y == 0) {
            ZERO
        }
        else {
            PointV1(x,y)
        }
        
    def apply (x: Int, y:Int) = create (x,y)
}

object PointApp extends App {
    val z = PointV1.ZERO
    val p = Point(_x=0,_y=0)
}

case class Point(x: Int, y: Int)
    def +(p: Point) = Point(x + p.x, y + p.y)
    
    // existe el copy para que te devuelva una igual con algo distinto si queres (inmutabilidad)

object PointApp extends App {
    val p = Point(3,4)
    
    val result = p + p
```

Instances -> apunta a Type class
(given)

Type class
(trait/extensions)

Algoritmo genérico -> apunta a Type class

User -> apunta a las otras 3

Ejemplo de Numeric
Type class -> Numeric
Algoritmo genérico -> sum
Instances -> Numeric[Int]
User -> Point, Numeric[Point] (como usuario puedo hacer que Point se comporte como Numeric)