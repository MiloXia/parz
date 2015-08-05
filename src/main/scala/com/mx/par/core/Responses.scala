package com.mx.par.core

import scala.collection.immutable.Map

import com.mx.fp.State
import com.mx.fp.core.Monoid

case class Responses[F[_]](store: Map[F[_], Any])
object Responses {//state
  def fetch[F[_], A](req: F[A], resp: Responses[F]): A = State[Responses[F], A] { s =>
    (s.store(req).asInstanceOf[A], s)
  }.run(resp)._1

  def add[F[_]](req: F[_], value: Any): Responses[F] = State[Responses[F], Unit] { s =>
    ((), Responses[F](Map[F[_], Any](req -> value) ++ s.store))
  }.run(Monoid[F].zero)._2

  implicit def Monoid[F[_]]: Monoid[Responses[F]] = new Monoid[Responses[F]] {
    def op(resp1: Responses[F], resp2: Responses[F]) = (resp1, resp2) match {
      case (Responses(r1), Responses(r2)) => Responses[F](r1 ++ r2)
    }
    val zero: Responses[F] = Responses[F](Map[F[_], Any]())
  }
}
