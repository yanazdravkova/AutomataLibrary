package automata

import scala.annotation.tailrec

sealed trait Chain[+A] {
  def head: A
  def tail: Option[Chain[A]]

  def isEmpty: Boolean = false

  def +:[B >: A](front: B): Chain[B] = Singleton(front) ++ this

  def :+[B >: A](back: B): Chain[B] = this ++ Singleton(back)

  def ++[B >: A](right: Chain[B]): Chain[B] = Append(this, right)

  def foldLeft[B](initial: B)(f: (B, A) => B): B = this match {
    case Singleton(element) => f(initial, element)
    case Append(left, right) =>
      val a = left.foldLeft(initial)(f)
      right.foldLeft(a)(f)
  }

  //  def foldLeftTailRecursive[B](initial: B)(f: (B, A) => B): B = {
  //    @tailrec
  //    def foldLeft(current: Chain[A], rest: List[Chain[A]], acc: B): B = current match {
  //      case Singleton(a) =>
  //        val newAcc = f(acc, a)
  //
  //        if (rest.isEmpty) newAcc
  //        else foldLeft(rest.head, rest.tail, newAcc)
  //      case Append(left, right) =>
  //        foldLeft(left, right :: rest, acc)
  //    }
  //
  //    foldLeft(this, List.empty, initial)
  //  }

  def reduceLeft[B >: A](f: (B, A) => B): B = this.listify match {
    case Singleton(first) => first
    case Append(Singleton(first), rest) => rest.foldLeft(first: B)(f)
    case _ => sys.error("Unexpected listify format")
  }

  def map[B](f: A => B): Chain[B] = flatMap(a => Singleton(f(a)))

  def flatMap[B](f: A => Chain[B]): Chain[B] = this match {
    case Singleton(element) => f(element)
    case Append(left, right) => Append(left.flatMap(f), right.flatMap(f))
  }

  def foreach(f: A => Unit): Unit = foldLeft(())((_, next) => f(next))

  override def equals(that: Any): Boolean = that match {
    case c: Chain[_] => this.toList == c.toList
    case _ => false
  }

  override def hashCode: Int = foldLeft(0) {
    _ * 31 + _.hashCode
  }

  override def toString: String = toList.mkString("Chain(", ",", ")")

  def toList: List[A] = foldLeft(List.empty[A])((acc, next) => next :: acc).reverse
  def toSet[B >: A]: Set[B] = foldLeft(Set.empty[B])((acc, next) => acc + next)

  def min[B >: A](implicit order: Ordering[B]): B = reduceLeft(order.min)
  def max[B >: A](implicit order: Ordering[B]): B = min(order.reverse)

  def listify: Chain[A] = this match {
    case Singleton(_) => this
    case Append(left @ Singleton(_), right) => Append(left, right.listify)
    case Append(Append(lleft, lright), right) => Append(lleft, Append(lright, right)).listify
  }
}

case class Singleton[+A](head: A) extends Chain[A] {
  def tail: Option[Chain[A]] = None
}
case class Append[+A](left: Chain[A], right: Chain[A]) extends Chain[A] {
  def head: A = left.head
  def tail: Option[Chain[A]] = left match {
    case Singleton(_) => Some(right)
    case _ => listify.tail
  }
}

object Chain {
  def apply[A](head: A, rest: A*): Chain[A] = rest.foldLeft(Singleton(head): Chain[A])(_ ++ Singleton(_))

  // Allows Chain to be used in pattern matching
  def unapplySeq[A](chain: Chain[A]): Option[Seq[A]] = Some(chain.toList)
}
