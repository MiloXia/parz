package com.mx.fp.core

import scala.xml.{XML, NodeSeq}

trait Free[F[_],A] {
  def unit(a: A): Free[F,A] = Return(a)
  def flatMap[B](k: A => Free[F,B]): Free[F,B] = this match {
    case Return(a) =>
      k(a)
    case Bind(command, cont) =>
      Bind(command, cont andThen (_ flatMap k))
  }
  def map[B](f: A => B): Free[F,B] =
    flatMap(f andThen (Return(_)))

  def foldMap[G[_]: Monad](f: F ~> G): G[A] = {
    val G = implicitly[Monad[G]]
    this match {
      case Return(a) =>
        G.unit(a)
      case Bind(command, cont) =>
        G.flatMap(f(command))(cont andThen (_ foldMap f))
    }
  }
}
final case class Return[F[_], A](a: A) extends Free[F,A]
final case class Bind[F[_], R, A](command: F[R],
                                  cont: R => Free[F,A]) extends Free[F,A]

trait ~>[F[_],G[_]] {
  def apply[A](fa: F[A]): G[A]
}

object Free {
  def lift[F[_],R](command: F[R]): Free[F,R] =
    Bind(command, (x:R) => Return(x))
}
object NewsLetterOpTest extends App {

  trait NewsLetterOp[A]
  case class GetSubject(s: String) extends NewsLetterOp[String]
  case class GetContent(s: String) extends NewsLetterOp[NodeSeq]
  case object InitModel extends NewsLetterOp[Unit]
  case class GetCollName(s: String) extends NewsLetterOp[String]
  //case class CreateCollection(name: String) extends NewsLetterOp[DBCollection]
  case class Send(subject: String, content: NodeSeq) extends NewsLetterOp[Unit]

  type FreeNewsLetter[A] = Free[NewsLetterOp, A]
  implicit def liftNewsLetter[A](n: NewsLetterOp[A]): FreeNewsLetter[A] = Free.lift(n)

  //Id
  sealed case class Id[A](a: A)
  implicit val IdMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: A) = Id(a)
    def flatMap[A,B](m: Id[A])(k: A => Id[B]): Id[B] =
      m match { case Id(a) => k(a) }
  }
  //Effect
  object MailEffect extends (NewsLetterOp ~> Id) {
    def apply[A](nl: NewsLetterOp[A]): Id[A] = nl match {
      case GetSubject(s) =>
        Id("todo")
      case GetContent(s) =>
        Id(XML.loadString("<a>todo</a>"))
      case GetCollName(s) =>
        Id("todo")
      case InitModel =>
        Id(println("iniModel"))
      case Send(s, c) =>
        Id(println(s"send $s, $c"))
    }
  }
  //Option
  implicit val OptionMonad = Monad.optionMonad
  object MailEffect2 extends (NewsLetterOp ~> Option) {
    def apply[A](nl: NewsLetterOp[A]): Option[A] = nl match {
      case GetSubject(s) =>
        Some("todo")
      case GetContent(s) =>
        Some(XML.loadString("<a>todo</a>"))
      case GetCollName(s) =>
        Some("todo")
      case InitModel =>
        Some(println("iniModel"))
      case Send(s, c) =>
        Some(println(s"send $s, $c"))
    }
  }

  val doo: FreeNewsLetter[Unit] = for {
    subject <- GetSubject("./a.txt")
    content <- GetContent("./a.txt")
    _ <- InitModel
    collName <- GetCollName("./a.txt")
    //    coll <- CreateCollection("name")
    _ <- Send(subject, content)
  } yield ()

  doo.foldMap(MailEffect)
  doo.foldMap(MailEffect2)
}
