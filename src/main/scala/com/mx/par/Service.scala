package com.mx.par

import com.mx.par.ParTest3.{Responses, Get, Request}

/**
 * Created by milo on 15-7-24.
 */
trait Service[R] {
  def deal[A](req: Request[A]): Responses
}

object Service {
  implicit object GetService extends Service[Get] {
    def deal(req: Request[Get]): Responses = {
      Responses.add(req) //TODO
    }
  }
  def fetch[R: Service](req: R) = {
    implicitly[R].deal(req)
  }
}
