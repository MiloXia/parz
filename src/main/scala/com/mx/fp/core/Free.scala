package com.mx.fp.core

trait Free[F[_],A] {
  //Monad
  def unit(a: A) = Return(a)
  def flatMap[B](f: A => Free[F,B]): Free[F,B] = this match {
    case Return(a) => f(a)
    case Bind(fa,g) => Bind(fa, (x: Any) => g(x) flatMap f)
    //case Bind(fa,g) => Bind(fa, g andThen (_ flatMap f))
  }
  def map[B](f: A => B): Free[F,B] = flatMap(a => Return(f(a)))
  //Interpreter
  def foldMap[G[_]: Monad](f: F ~> G): G[A] = this match {
    case Return(a) => implicitly[Monad[G]].unit(a)
    case Bind(b,g) => implicitly[Monad[G]].flatMap(f(b))(a => g(a).foldMap(f))
  }
}
case class Return[F[_],A](a: A) extends Free[F,A] //表示计算结束
case class Bind[F[_],I,A](a: F[I], f: I => Free[F,A]) extends Free[F,A]
trait ~>[F[_],G[_]] {
  def apply[A](fa: F[A]): G[A]
}
object FreeTest extends App {
  trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]
  implicit def lift[F[_],A](fa: F[A]): Free[F,A] = Bind(fa, (a: A) => Return(a))
  val prg = for {
    x <- Ask("What's your first name?")
    y <- Ask("What's your last name?")
    _ <- Tell(s"Hello $y $x!")
  } yield ()
  type Id[A] = A
  implicit val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: A) = a
    def flatMap[A,B](fa: A)(f: A => B): B = f(fa)
  }
  object Console extends (Interact ~> Id) {
  def apply[A](i: Interact[A]): A = i match {
      case Ask(prompt) => {
        println(prompt)
        readLine
      }
      case Tell(msg) => println(msg)
    }
  }
  prg.foldMap(Console)
}