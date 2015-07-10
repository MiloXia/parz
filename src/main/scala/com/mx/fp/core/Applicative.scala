package com.mx.fp.core

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
    apply(fb)(map(fa)(f.curried)) //map(fa)(a => (b => c)) >>> F[A=>B]
  }
  def apply[A,B](fa: F[A])(fab: F[A =>B]): F[B] = { //<*>
    map2(fab,fa)((f,a) => f(a))
  }
  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    map2(unit(f),fa)((f,a) => f(a))
  }
  //    def map[A,B](fa: F[A])(f: A => B): F[B] = {
  //      apply(fa)(unit(f))
  //    }
  //map3(ma,mb,mc)(f) = apply(mc)(apply(mb)(apply(mc)(unit(f.curried))))
  def map3[A,B,C,D](ma: F[A], mb: F[B], mc: F[C])(f: (A,B,C) => D): F[D] = {
    apply(mc)(apply(mb)
      (apply(ma)(unit(f.curried))))
  }
  def map4[A,B,C,D,E](ma: F[A], mb: F[B], mc: F[C],md: F[D])(f: (A,B,C,D) => E): F[E] = {
    apply(md)(apply(mc)
      (apply(mb)
        (apply(ma)(unit(f.curried)))))
  }
}
object Applicative {
  val optionApplicative = new Applicative[Option] {
    def unit[A](a: A) = Option(a)

    override def map2[A, B, C](ma: Option[A], mb: Option[B])(f: (A, B) => C): Option[C] = (ma, mb) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }
  }
//  val s = optionApplicative.apply(Some(1))(Some[Int => Int](_ + 1))
//  println(s)
//
//  implicit class OptionApplicativeOp[A](opt1: Option[A]) {
//    def apply[B, C](opt2: Option[A => B]): Option[B] =
//      optionApplicative.map2(opt1, opt2)((opt1x, opt2f) => opt2f(opt1x))
//
//    def apply2[B, C](opt2: Option[(A, B) => C]): Option[B => C] =
//      optionApplicative.map2(opt1, opt2)((opt1x, opt2f) => opt2f(opt1x, _))
//  }
//
  def inc(x: Int) = x + 1

  def add2(x: Int, y: Int) = x + y

  def add3(x: Int, y: Int, z: Int) = x + y + z
//
//  val s2 = Some(1) apply Some(inc _)
//  println(s2)
//  val s3 = optionApplicative.map3(Some(1), Some(2), Some(3))(add3)
//  println(s3)
//  val s4 = Some(1) apply (Some(2) apply2 Some(add2 _))
//  println(s4)
  implicit class OptionApplicativeOp1[A,B](opt1: Option[A => B]) {
    def apply(opt2: Option[A]): Option[B] =
      optionApplicative.map2(opt2, opt1)((opt2x, opt1f) => opt1f(opt2x))
  }
  implicit class OptionApplicativeOp2[A,B,C](opt1: Option[(A, B) => C]) {
    def apply(opt2: Option[A]): Option[B => C] =
      optionApplicative.map2(opt2, opt1)((opt2x, opt1f) => opt1f(opt2x, _))
  }
  val s5 = Some(add2 _) apply Some(2) apply Some(1)
}