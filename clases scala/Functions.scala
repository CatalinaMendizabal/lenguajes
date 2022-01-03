object Functions extends App:

  def mul(n1: Int, n2: Int): Int = n1 * n2
  mul(2,2)

  def mul2(n1: Int)(n2: Int): Int = n1 * n2
  mul2(2)(2)

  val double1: Int => Int = mul2(2) //me devuelve una funcion con un unico parametro. Purificacion
  val double2: Function1[Int, Int] = mul2(2)

  double1(5)

  // funciones de alt orden (reciben funciones)
  def f1 (p:Int => Int): Unit = {
    println("p: "+ p(5))
  }

  f1(double1)
  f1((n: Int) => n +1)
  f1(n => n +1)
  f1(_ +1)

  val partialFunction: PartialFunction[Any, Int] =
    case n: Int => n + 1
    case s: String => s.length + 1

  println(partialFunction(5))
  println(partialFunction("hola"))

  def completeFunction(v: Any): Int = {
    v match {
      case n: Int =>
        n + 1
      case s: String =>
        s.length + 1
    }
  }

  println(completeFunction(5))
  println(completeFunction("hola"))

  //  println(partialFunction(true))
  //  println(completeFunction(true))
  // explotan

  if(partialFunction.isDefinedAt((true))) then
    partialFunction(true)

  val list = List("hello", 12, true, 4)

  println(list.collect(partialFunction)) // le va a preguntar antes si la soporta o no

  println(list.collect {
    case n: Int =>
      n * 2
  })

  println(list.collectFirst {
    case n: Int =>
      n + 1
  })

  def f1(p: Int): Unit = {
    println("f1.start")
    p
    p
    println("f1.end")
  }

  f1({
    println("Generating value for f1")
    10
  })

  def f2(p: () => Int): Unit = {
    println("f2.start")
    p()
    p()
    println("f2.end")
  }

  f2(() => {
    println("Generating value for f2")
    10
  })

  //pasaje de parametro por nombre
  def f3(p: => Int): Unit = {
    println("f3.start")
    p // lo evalua envuelto en un try catch
    p
    println("f3.end")
  }

  f3 {
    println("Generating value for f3")
    10
  }

  object MyLog {
    var enabled = false
    def logDebug(msg: => String): Unit = {
      if (enabled) {
        println(s"DEBUG $msg")
      }
    }
  }

  val users = List("Juan", "Pepe")
  MyLog.enabled = true
  MyLog.logDebug(s"Usuarios conectados: $users") // si le pones enabled = false igual lo hace a esto, pero no te lo muestra (si no le pones msg: => String)

