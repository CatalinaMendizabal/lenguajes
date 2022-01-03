object ForComprehentions extends App:
  val numbers = List(1,2,3,4,5)

  for elem <- numbers if elem != 3 do
    println(elem)

  val newList1 = for elem <- numbers if elem != 3
    yield elem + 1

  println(newList1)

  val newList2 = for
    i <- 0 until 10
    j <- 0 until 10 if i != j
  yield (i, j)

  println(newList2)

  val map = Map("hello" -> "hola", "bye" -> "chau")

  for (k, v) <- map do
    println(s"$k -> $v")