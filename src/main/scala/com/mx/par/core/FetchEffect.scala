package com.mx.par.core

import Implicit._
/**
 * Created by milo on 15-8-5.
 */
class FetchEffect[F[_] <: Request[_]](service: Service[F]) extends (Request ~> Id) { //F[A] is Requests[Responses]
  def apply[A](nl: Request[A]): Id[A] = nl match {
    case Requests(l) =>
      if(l.length > 1) println(s"Do [${l.mkString(",")}] in parallel")
      fetch(l)
    case _ => throw new Exception("bad cmd")
  }

  val f: F[_] => Responses[Request] = r => {
    println(s"--${Thread.currentThread().getName}")
    service.deal(r)
  }

  def fetch(l: List[Request[_]]): Responses[Request] = {
    l.par.map(x => f(x.asInstanceOf[F[_]])).fold(Responses.Monoid.zero)(Responses.Monoid.op)
  }
}

//service
trait Service[F[_] <: Request[_]] {
  def deal[A](req: F[A]): Responses[Request] = Responses.add[Request](req, fetch(req))
  def fetch[A](req: F[A]): A
}
