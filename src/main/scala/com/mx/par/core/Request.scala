package com.mx.par.core

import com.mx.fp.core.Monoid

trait Request[A]
//Monoid
case class Requests(l: List[Request[_]]) extends Request[Responses[Request]]
object Requests {
  def add[A](req: Request[A]): Requests = Requests(req :: Nil)
  def empty = Requests(Nil)
}