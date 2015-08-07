package com.mx.par

import scala.util.Random

/**
 * Created by milo on 15-8-7.
 */
object FCPDemo extends App {
  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global

  def debug(msg: String) = println(msg + " : " + Thread.currentThread().getName)

  case class Task(i: Int)
  val taskList = List(Task(1), Task(2), Task(3), Task(4))

  def forkjoin[T, A](taskList: List[T], doo: => A): List[A] = {
    val fs = taskList.map(t => future{debug("do"); doo})
    val fpfs = fs zip taskList.map{ t =>
      val p = promise[Int]()
      (p, p.future)
    }
    fs foreach { f => f onSuccess {
        case r => fpfs.find(_._1 == f) match {
          case Some((f1, (p1, f2))) => {debug("success"); p1 success 1}
          case _ => throw new Exception("match error")
        }
      }
    }
    val count = Await.result(Future.sequence(fpfs.map(_._2._2)), Duration.Inf).sum
    if(count == taskList.length)
      Await.result(Future.sequence(fs), Duration.Inf)
    else throw new Exception("some failure")
  }

  println(forkjoin(taskList, Random.nextInt))

  Thread sleep 1000
}
