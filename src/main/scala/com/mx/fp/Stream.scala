package com.mx.fp

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  //other
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = {
      s.uncons match {
        case None => acc
        case Some((h,t)) => go(t,h :: acc)
      }
    }
    go(this, Nil).reverse
  }
  import Stream._
  def take(n: Int): Stream[A] = {
    if ( n == 0 ) empty
    else
      uncons match {
        case None => empty
        case Some((h,t)) => cons(h,t.take(n-1)) //lazy的 并没真的take 执行uncons时才计算
      }
  }
  def drop(n: Int): Stream[A] = {
    if (n == 0) this
    else {
      uncons match {
        case Some((h,t)) => t.drop(n-1)
        case _ => this
      }
    }
  }
  // lazy的foldRight
  def foldRight[B](z: B)(op: (A, => B) => B): B = { //op第二个参数是=> B，所以t.foldRight(z)(op)就是延迟计算的
    uncons match {
      case None => z
      case Some((h,t)) => op(h,t.foldRight(z)(op)) //只要不在op里使用b fold就会结束
    }
  }
  // lazy的exists 满足条件p(a) = true 就结束foldRight
  def exists(p: A => Boolean): Boolean = {
    foldRight(false){(a,b) => p(a) || b } //op = (a,b) => p(a) || b ，当p(a) = true时 b不再传给foldRight
  }
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true){(a,b) => p(a) && b}
  }
  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A]){(h,t) => if(p(h)) cons(h,t) else t} //该表达式，foldRight会执行到底
  }
  def map[B](p: A => B): Stream[B] = {
    foldRight(empty[B]){(h,t) => cons(p(h), t)}
  }
  def flatMap_1[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B]){(h,t) => f(h) #++ t}
  }
  def append[B >: A](b: Stream[B]): Stream[B] = {
    uncons match {
      case None => b
      case Some((h,t)) => cons(h, t.append(b))
    }
  }
  //append简写
  def #++[B >: A](b: Stream[B]): Stream[B] = append(b)
  //    override def toString = s"Stream(${toList.mkString(",")})" //会导致全部计算一遍，这可不好
  override def toString = "Stream(lazy...)"
}
object Stream {
  def empty[A]: Stream[A] = new Stream[A] { def uncons = None }
  //采用call-by-name比较好，Cons case class的构造参数会比较难看
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = new Stream[A] {
    def uncons = Some((h, t)) /*!!!!!!此时计算*/
  }
  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }
  //Infinite Stream
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def fibs: Stream[Int] = {
    def go (prev: Int, cur: Int): Stream[Int] = {
      cons(prev,go(cur,prev + cur))
    }
    go(0,1)
  }
  def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a,s)) => cons(a, unfold(s)(f))
    }
  }
  def constantByUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a,a)))
  def fromByUnfold(s: Int): Stream[Int] = unfold(s)(s => Some(s,s+1))
  def fibsByUnfold: Stream[Int] = unfold((0,1)){ case (a1,a2) => Some((a1, (a2, a1+a2))) }
}

object StreamTest extends App {
  def get1 = {println("hello1");1}
  def get2 = {println("hello2");2}
  def get3 = {println("hello3");3}
  println(Stream(1,2,3) take 2)
  println(Stream(get1,get2,get3) take 2)
  println((Stream(1,2,3) take 2).toList)
  println("------")
  println(Stream(1,2,3).foldRight(0)(_+_))
  println(Stream(1,2,3).foldRight(0)((a, b) => a ))
  println("------")
  println(Stream(1,2,3).take(2).foldRight(0)(_+_))
  println(Stream(1,2,3).exists(_ == 2))
  println("------")
  println(Stream(1,2,3).map(_+1).filter(_ % 2 == 0).toList)

  def getData(s:Int, l:Int) = Option(s"sql...skip=$s limit=$l")
  def getMore(skip: Int, limit: Int) = Stream.unfold(skip){
    s => Some(getData(s, limit), s+limit)
  }
  println(getMore(0,10).take(3).toList)
}