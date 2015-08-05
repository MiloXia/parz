package com.mx.par.core

import com.mx.fp.core.{Applicative, Monad, Monoid}

object Implicit {
  //request monoid
  implicit def reqMonoid: Monoid[Request[_]] = new Monoid[Request[_]] {
    def op(req1: Request[_], req2: Request[_]): Request[_] = (req1, req2) match {
      case (Requests(l1), Requests(l2)) => Requests(l1 ++ l2)
      case other => throw new Exception(s"bad request $other")
      //      case (Requests(l), r) => Requests(r :: l) //Impossible
      //      case (r, Requests(l)) => Requests(r :: l) //Impossible
      //      case (r1, r2) => Requests(r1 :: r2 :: Nil)//Impossible
    }
    val zero: Request[_] = Requests.empty
  }

  //lift
  implicit def dataFetch[A](req: Request[A]): Free[Request, A] =
    Blocked(Requests add req, (resp: Responses[Request]) => Done(Responses.fetch(req, resp)))

  //G[A]
  type Id[A] = A
  implicit val IdMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: A) = a
    def flatMap[A,B](m: Id[A])(k: A => Id[B]): Id[B] =
      m match { case a => k(a) }
  }

  //run
  implicit class ToFecthEffect[A](free: Free[Request, A]) {
    def run[F[_] <: Request[_]](implicit effect: FetchEffect[F]) = free.foldMap(effect)
  }

  def freeApplicative[F[_]] = new Applicative[({type f[A]=Free[F,A]})#f] {
    def unit[A](a: A): Free[F, A] = Done(a)
    def map2[A, B, C](mb1: Free[F, A], mb2: Free[F, B])(f: (A, B) => C)(implicit M: Monoid[F[_]]): Free[F, C] = (mb1, mb2) match {
      case (Done(a), Done(b)) =>
        Done(f(a, b))
      case (Done(a1), Blocked(reqs, cont)) =>
        Blocked(reqs, (x: Any) => Done(a1).map2(cont(x))(f))
      case (Blocked(reqs, cont), Done(b1)) =>
        Blocked(reqs, (x: Any) => cont(x).map2(Done(b1))(f))
      case (Blocked(reqs1, cont1), Blocked(reqs2, cont2)) =>
        Blocked(M.op(reqs1, reqs2), (x: Any) => cont1(x).map2(cont2(x))(f))
    }
  }
  implicit class FreeApplicativeOp1[F[_], A, B](free1: Free[F, A => B]) {
    def <*>(free2: Free[F, A])(implicit M: Monoid[F[_]]): Free[F, B] =
      freeApplicative.map2(free2, free1)((free2x, free1f) => free1f(free2x))
  }
  implicit class FreeApplicativeOp2[F[_], A, B, C](free1: Free[F, (A, B) => C]) {
    def <*>(free2: Free[F, A])(implicit M: Monoid[F[_]]): Free[F, B => C] =
      freeApplicative.map2(free2, free1)((free2x, free1f) => free1f(free2x, _))
  }
  implicit class FreeApplicativeOp3[F[_], A, B, C, D](free1: Free[F, (A, B, C) => D]) {
    def <*>(free2: Free[F, A])(implicit M: Monoid[F[_]]): Free[F, (B, C) => D] =
      freeApplicative.map2(free2, free1)((free2x, free1f) => free1f(free2x, _, _))
  }
  implicit class FreeApplicativeOp4[F[_], A, B, C, D, E](free1: Free[F, (A, B, C, D) => E]) {
    def <*>(free2: Free[F, A])(implicit M: Monoid[F[_]]): Free[F, (B, C, D) => E] =
      freeApplicative.map2(free2, free1)((free2x, free1f) => free1f(free2x, _, _, _))
  }
  implicit class FreeApplicativeOp5[F[_], A, B, C, D, E, F1, G](free1: Free[F, (A, B, C, D, F1) => G]) {
    def <*>(free2: Free[F, A])(implicit M: Monoid[F[_]]): Free[F, (B, C, D, F1) => G] =
      freeApplicative.map2(free2, free1)((free2x, free1f) => free1f(free2x, _, _, _, _))
  }
  implicit class FreeApplicativeOp6[F[_], A, B, C, D, E, F1, G, H](free1: Free[F, (A, B, C, D, F1, G) => H]) {
    def <*>(free2: Free[F, A])(implicit M: Monoid[F[_]]): Free[F, (B, C, D, F1, G) => H] =
      freeApplicative.map2(free2, free1)((free2x, free1f) => free1f(free2x, _, _, _, _, _))
  }
  //...
  def pure[F[_], A](a: A): Free[F, A] = freeApplicative.unit(a)
  implicit class FreeApplicativeOp[A, B](f: A => B) {
    def `<$>`(req: Request[A]): Free[Request, B] = dataFetch(req).map(f)
  }
}
