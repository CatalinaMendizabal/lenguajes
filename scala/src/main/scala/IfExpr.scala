@main def ifexpr = {
  val v = 1

  if(v == 1) {
    println("...")
    println("...")
  }

  if(v <= 1)
    println("...")
    println("...")

  if v <= 1 then
    println("...")
    println("...")

  val result = if v > 1 then 3 else 5
}