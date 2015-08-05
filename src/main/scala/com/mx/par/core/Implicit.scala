package com.mx.par.core

import com.mx.fp.core.{Monad, Monoid}

/**
 * Created by milo on 15-8-5.
 */
object Implicit {
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

  implicit def dataFetch[A](req: Request[A]): Free[Request, A] =
    Blocked(Requests add req, (resp: Responses[Request]) => Done(Responses.fetch(req, resp)))

  type Id[A] = A
  implicit val IdMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: A) = a
    def flatMap[A,B](m: Id[A])(k: A => Id[B]): Id[B] =
      m match { case a => k(a) }
  }
}
