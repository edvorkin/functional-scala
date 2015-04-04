import com.sun.xml.internal.bind.v2.runtime.unmarshaller.XsiNilLoader
import fpinscala.datastructures.Cons

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  }

  def product(ds: List[Double]): Double = ds match {
    case Nil =>  1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def removeFirst[A](ds: List[A]):List[A] = ds match {
    case Nil => Nil
    case Cons(x, xs)=> xs
  }

  def setHead[A](head:A, ds: List[A]): List[A] = ds match {
    case Nil=>Nil
    case Cons(x,xs) => Cons(head,xs)
  }

  def drop[A](l: List[A], n: Int): List[A]= {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs)  if f(x) => dropWhile(xs,f)
    case _ => l
  }
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

}


List.product(List(1,2))
val t=List(1,2,3,4,5)
val t1=List.removeFirst(Nil)
List.sum(t)
List.setHead(22,t)
List.setHead("aa",List("a","b","c"))
List.drop(t,2)
List.init2(t)
