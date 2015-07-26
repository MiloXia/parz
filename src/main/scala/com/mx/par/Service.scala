package com.mx.par

import com.mx.par.ParTest3.{Responses, Get, Request}

/**
 * Created by milo on 15-7-24.
 */
trait Service[Req] {
  def deal(req: Req): Responses
}

object Service {
  implicit object GetService extends Service[Get] {
    def deal(req: Get): Responses = req match {
      case Get(q) => Responses.add(req, "aaa")
    }
  }
  def fetch[R: Service](req: R) = {
    implicitly[Service[R]].deal(req)
  }
}
