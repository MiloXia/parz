package com.mx.fp.core

trait Functor[F[_]] {
  def map[A,B](a: F[A])(f: A => B): F[B]
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
  def map[A,B](ma: M[A])(f: A => B): M[B] = {
    flatMap(ma){a => unit(f(a))}
  }
  //Applicative
  //    def map2[A,B,C](ma: M[A], mb: M[B])(f: (A,B) => C): M[C] = {
  //      flatMap(ma) { a => map(mb){ b => f(a,b) }}
  //    }
  //    def map2[A,B,C](ma: M[A], mb: M[B])(f: (A,B) => C): M[C] = {
  //      flatMap(ma) { a => map(mb){ b => f(a,b) }}
  //    }
  //    def map3[A,B,C,D](ma: M[A], mb: M[B], mc: M[C])(f: (A,B,C) => D): M[D] = {
  //      map2(ma,
  //        map2(mb,mc){(b,c) => (b,c)}
  //      ){(a,bc) => {
  //        val (b,c) = bc
  //        f(a,b,c)
  //      }}
  //    }
  //    def map4[A,B,C,D,E](ma: M[A], mb: M[B], mc: M[C], md: M[D])(f: (A,B,C,D) => E): M[E] = {
  //      map2(ma,
  //        map2(mb,
  //          map2(mc,md){(c,d) => (c,d)}
  //        ){(b,cd) => (b,cd)}
  //      ){(a,bcd) => {
  //        val (b,(c,d)) = bcd
  //        f(a,b,c,d)
  //      }}
  //    }
}

//trait Monad[M[_]] extends Applicative[M]{
//  def unit[A](a: A): M[A]
//  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = {
//    join(map(ma)(f))
//  }
//  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = {
//    a => { flatMap(f(a))(g)}
//  }
//  def join[A](mma: M[M[A]]): M[A] = {
//    flatMap(mma)(ma => ma)
//  }
//  override def apply[A,B](ma: M[A])(fab: M[A => B]): M[B] = {
//    flatMap(fab)(f => flatMap(ma)(a => unit(f(a))))
//  }
//}

object ListFunctor extends Functor[List] {
  def map[A,B](la: List[A])(f: A => B): List[B] = la map f
}
object OptionFunctor extends Functor[Option] {
  def map[A,B](oa: Option[A])(f: A => B): Option[B] = oa map f
}
//println(ListFunctor.map(List(1,2,3)){_ + 10})
//println(OptionFunctor.map(Some(1)){_ + 10})
//
object Monad {
  val listMonad = new Monad[List] {
    def unit[A](a: A) = List(a)
    def flatMap[A,B](la: List[A])(f: A => List[B]): List[B] = {
      la flatMap f
    }
  }
  val optionMonad = new Monad[Option] {
    def unit[A](a: A) = Some(a)
    def flatMap[A,B](oa: Option[A])(f: A => Option[B]): Option[B] = {
      oa flatMap f
    }
  }
}


