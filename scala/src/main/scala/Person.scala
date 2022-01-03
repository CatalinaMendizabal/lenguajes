case class Persona(nombre: String, apellido: String, edad: Int,
                   hijos: List[Persona]):

  def descendientes: List[Persona] = this :: hijos.flatMap(_.descendientes)

  def menoresYMayores: (List[Persona], List[Persona]) = descendientes.partition(_.edad > 18)

  def sinHijos: List[Persona] = descendientes.filter(_.hijos.isEmpty)

  def conHijos: List[Persona] = descendientes.filter(_.hijos.nonEmpty)

  def hijosPorEdad: List[Persona] = hijos.sortBy(_.edad)

  def hijosConsecutivos: List[(Persona, Persona)] = hijosPorEdad.zip(hijosPorEdad.tail)

  def hijosMellizos: List[(Persona, Persona)] = hijosConsecutivos.filter((h1, h2) => h1.edad == h2.edad)
  def mellizos: List[(Persona, Persona)] = conHijos.flatMap(_.hijosMellizos)

  def hijosDifCuatro: List[(Persona, Persona)] = hijosConsecutivos.filter((h1, h2) => h2.edad - h1.edad > 4)
  def difCuatro: List[(Persona, Persona)] = conHijos.flatMap(_.hijosDifCuatro)

  def promedioEdadHijos: Int = hijos.foldLeft(0)(_ + _.edad) / hijos.size
  def conHijosPromEdad(prom: Int): List[Persona] = conHijos.filter(_.promedioEdadHijos == prom)


