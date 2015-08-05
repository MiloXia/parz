package com.mx.par

import scala.util.Random

import com.mx.par.core.{Service, FetchEffect, Request}
import com.mx.par.core.Implicit._

/**
 * Created by milo on 15-8-5.
 */
object day08 extends App {
  class GotoWork[A] extends Request[A]
  case class Breakfast(egg: Int, milk: Int)
  case object GetUp extends GotoWork[Unit]
  case object Wash extends GotoWork[Unit]
  case object MakeBreakfast extends GotoWork[Breakfast]
  case class Eat(breakfast: Breakfast) extends GotoWork[Unit]
  case object GoOut extends GotoWork[Unit]
  case object TakeBus extends GotoWork[Unit]

  class IOReq[A] extends Request[A]
  case class Get(q: String) extends IOReq[String]
  case class Put(r: String) extends IOReq[Unit]

  val early = for {
    _ <- GetUp
    _ <- Wash
    breakfast <- MakeBreakfast
    _ <- Eat(breakfast)
    _ <- GoOut
    _ <- TakeBus
  } yield ()

  val late = for {
    _ <- GetUp
    _ <- Wash
    breakfast <- MakeBreakfast
    _ <- Eat(breakfast).map2(GoOut)((_:Unit, _:Unit) => ())
    _ <- TakeBus
  } yield ()


  val test = for {
    a <- Get("get a")
    d <- Get("get b").map2(Get("get c"))((b: String, c: String) => b + c)
    _ <- Put(a+d)
  } yield ()

  object GotoWork {
    implicit object DataService extends Service[GotoWork] {
      override def fetch[A](req: GotoWork[A]): A = req match {
        case GetUp =>
          println("get up")
        case Wash =>
          println("wash")
        case MakeBreakfast =>
          println("make breakfast")
          Breakfast(2, 1)
        case Eat(b) =>
          println(s"eat $b")
        case GoOut =>
          println(s"go out")
        case TakeBus =>
          println("take a bus")
      }
    }
  }

  object IOReq {
    implicit object DataService extends Service[IOReq] {
      override def fetch[A](req: IOReq[A]): A = req match {
        case Get(q) =>
          println(q)
          Random.nextString(10)
        case Put(s) =>
          println(s)
      }
    }
  }

  early.foldMap(new FetchEffect(GotoWork.DataService))
  println("-" * 20)
  late.foldMap(new FetchEffect(GotoWork.DataService))
  println("-" * 20)
  test.foldMap(new FetchEffect(IOReq.DataService))

}
