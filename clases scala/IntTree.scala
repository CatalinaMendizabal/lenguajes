sealed abstract class IntTree
//  def sum: Int

object TreeLib:
  def sumLeaves(t: IntTree): Int =
    t match
      case Branch(l, r) => sumLeaves(l) + sumLeaves(r) //patermachin en vez del sum de mierda de OOP
      case Leaf(x) => x

  extension (t: IntTree)
    def sum = sumLeaves(t)

case class Branch(left: IntTree, right: IntTree) extends IntTree
//  override def sum: Int = left.sum + right.sum

case class Leaf(x: Int) extends IntTree
//  override def sum: Int = x

object TreeApp extends App:
  val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

  import TreeLib.*

  println(tree)
  println(tree.sum)
  println(sumLeaves(tree))
