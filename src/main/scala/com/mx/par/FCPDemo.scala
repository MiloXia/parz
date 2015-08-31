package com.mx.par

import scala.util.control.NonFatal
import scala.util.{Try, Failure, Success, Random}

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

  def forkjoin[T, A](taskList: List[T], doo: T => A): List[A] = {
    val fs = taskList.map(t => future{debug("do"); doo(t)})
    val fpfsForSuccess = fs zip taskList.map{ t =>
      val p = promise[Int]()
      (p, p.future)
    }
    val fpfsForFailure = fs zip taskList.map{ t =>
      val p = promise[(T, Throwable)]()
      (t, p, p.future)
    }
    fs foreach { f => f onComplete  {
        case Success(r) => fpfsForSuccess.find(_._1 == f) match {
          case Some((f1, (p1, f2))) => debug("success"); p1 success 1
          case _ => throw new Exception("match error")
        }
        case Failure(e) => fpfsForFailure.find(_._1 == f) match {
          case Some((f1, (t, p1, f2))) => debug("failure"); p1 success (t, e)
           /*e match {
            case NonFatal(error) => debug("nonfatal failure"); p1 success error //can retry
            case other => debug("failure");  p1 success other //need exit
          }*/
          case _ => throw new Exception("match error")
        }
      }
    }
    val count = Try(Await.result(Future.sequence(fpfsForSuccess.map(_._2._2)), 2 seconds).sum) match {
      case Success(r) => r
      case Failure(e) => 0
    }
    println("count : " + count)
    if(count == taskList.length)
      Await.result(Future.sequence(fs), Duration.Inf)
    else {
      val errorList = Await.result(Future.sequence(fpfsForFailure.map(_._2._3)), Duration.Inf)
      //TODO retry
      throw new Exception(errorList.map{ case (t, e) => t.toString + "failure" + " cause: " + formatException(e)}.mkString("\n\t"))
    }
  }

  def formatException(e: Throwable): String = {
    val newLine = "\n\t"
    "Message: " + e.toString + newLine +
      e.getStackTrace.map(_.toString).mkString(newLine) + newLine + {
      e.getCause match {
        case cause: Throwable => newLine + "Caught and thrown by:" + newLine + formatException(cause)
        case _ => ""
      }
    }
  }

  def doo(t: Task) = t match {
    case Task(i) => if(i == 3) throw new Exception("aa") else i
  }

  println(forkjoin(taskList, doo))

  Thread sleep 1000
}
