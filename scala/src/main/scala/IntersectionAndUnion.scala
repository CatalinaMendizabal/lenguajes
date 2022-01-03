trait Identificable:
  def id: Int

trait Named:
  def name: String

trait A:
  def children: List[A]

trait B:
  def children: List[B]

class C extends A, B:
  def children: List[A & B] = Nil // intersection type

case class Person1(id: Int, name: String, age:Int) extends Identificable, Named

object IntersectionAndUnion extends App :

    val list1 : List[Identificable] = Nil
    val list2 : List[Named] = Nil

    // union type
    val list3: List[Identificable | Named] = list1 ++ list2

    // literal types
    val n: 3 = 3
    val z: "hello" = "hello"

    def op(p1: String, operator: "+" | "-" | "*" | "/", p2: String) = {

    }

    op("45", "+", "213")
    op("45", "-", "213")
    op("45", "*", "213")


    case class UserPassword(user: String, pwd: String)
    case class Token(hash: String)

    def authentication(credentials: UserPassword | Token): Boolean =
      credentials match
        case UserPassword(u,p) =>
          true
        case Token(key) =>
          true
