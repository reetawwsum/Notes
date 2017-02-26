abstract class InSet {
  def incl(x: Int): InSet
  def contains(x: Int): Boolean
  def union(other: InSet): InSet
}

object Empty extends InSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): InSet =
    new NonEmpty(x, Empty, Empty)

  def union(other: InSet): InSet = other

  override def toString: String = "."
}

class NonEmpty(elem: Int, left: InSet, right: InSet) extends InSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): InSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def union(other: InSet): InSet = {
    ((left union right) union other) incl elem
  }

  override def toString: String = "{" + left + elem + right + "}"
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4
val t3 = t2 incl 2
