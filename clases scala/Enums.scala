enum WeekDay:
  case Mon, Tue, Wed, Thu, Fri, Sat, Sun

enum Color(val rgb: Int):
  case Red extends Color(0xFF0000)
  case Green extends Color(0x00FF00)
  case Blue extends Color(0x0000FF)

@main def enums =
  import WeekDay.*

  val day: WeekDay = Mon

  day match
    case Sat | Sun => "08-12hs"
    case _ => "08-18hs"


