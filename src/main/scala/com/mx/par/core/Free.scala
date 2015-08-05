package com.mx.par.core

import com.mx.fp.core.{Monad, Monoid}

trait Free[F[_],A] {
  def unit(a: A): Free[F,A] = Done(a)
  def flatMap[B](k: A => Free[F,B]): Free[F,B] = this match {
    case Done(a) =>
      k(a)
    case Blocked(reqs, cont) =>
      Blocked(reqs, cont andThen (_ flatMap k))
  }
  def map[B](f: A => B): Free[F,B] =
    flatMap(f andThen (Done(_)))
  //Applicative
  def map2[B, C](mb: Free[F, B])(f: (A, B) => C)(implicit M: Monoid[F[_]]): Free[F, C] = (this, mb) match {
    case (Done(a), Done(b)) =>
      Done(f(a, b))
    case (Done(a1), Blocked(reqs, cont)) =>
      Blocked(reqs, (x: Any) => Done(a1).map2(cont(x))(f))
    case (Blocked(reqs, cont), Done(b1)) =>
      Blocked(reqs, (x: Any) => cont(x).map2(Done(b1))(f))
    case (Blocked(reqs1, cont1), Blocked(reqs2, cont2)) =>
      Blocked(M.op(reqs1, reqs2), (x: Any) => cont1(x).map2(cont2(x))(f))
  }

  def apply[B](fab: Free[F, A => B])(implicit M: Monoid[F[_]]): Free[F, B] = { //<*>
    this.map2(fab)((a, f) => f(a))
  }

  def foldMap[G[_]: Monad](f: F ~> G): G[A] = {
    val G = implicitly[Monad[G]]
    this match {
      case Done(a) =>
        G.unit(a)
      case Blocked(reqs, cont) =>
        G.flatMap(f(reqs))(cont andThen (_ foldMap f))
    }
  }
}

final case class Done[F[_], A](a: A) extends Free[F,A]
final case class Blocked[F[_], R, A](reqs: F[R], cont: R => Free[F,A]) extends Free[F,A]

trait ~>[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}
