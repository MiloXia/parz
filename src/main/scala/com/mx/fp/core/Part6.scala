package com.mx.fp.core

import scala.annotation.tailrec

object Part6 { //error https://github.com/runarorama/Days2012/issues/1

  sealed trait Free[S[+_],+A] {
    private case class FlatMap[S[+_],A,+B](a: () => Free[S,A],f: A => Free[S,B]) extends Free[S,B]

    def map[B](f: A => B): Free[S,B] =
      flatMap(x => Done(f(x)))

    def flatMap[B](f: A => Free[S,B]): Free[S,B] =
      this match {
        case FlatMap(a, g) =>
          FlatMap(a, (x: Any) => g(x) flatMap f)
        case x => FlatMap(() => x, f)
      }

    @tailrec final def resume(implicit S: Functor[S]): Either[S[Free[S, A]], A] =
      this match {
        case Done(a)  => Right(a)
        case More(k) => Left(k)
        case a FlatMap f  => a() match {
          case Done(a)  => f(a).resume
          case More(k) => Left(S.map(k)(_ flatMap f))
          case b FlatMap g  => b().flatMap((x: Any) =>
            g(x) flatMap f).resume
        }
      }
  }

  case class Done[S[+_],+A](a: A) extends Free[S,A]
  case class More[S[+_],+A](k: S[Free[S,A]]) extends Free[S,A]

  trait Functor[F[_]] {
    def map[A,B](m: F[A])(f: A => B): F[B]
  }

  sealed trait StateF[S,+A]
  case class Get[S,A](f: S => A) extends StateF[S,A]
  case class Put[S,A](s: S, a: A) extends StateF[S,A]

  implicit def statefFunctor[S] = new Functor[({type λ[+α] = StateF[S,α]})#λ] {
    def map[A,B](m: StateF[S, A])(f: A => B) = m match {
      case Get(g) => Get((s:S) => f(g(s)))
      case Put(s, a) => Put(s, f(a))
    }
  }

  type FreeState[S,+A] = Free[({type λ[+α] = StateF[S,α]})#λ, A]

  def pureState[S,A](a: A): FreeState[S,A] = Done[({type λ[+α] = StateF[S,α]})#λ, A](a)
  def getState[S]: FreeState[S,S] = More[({type λ[+α] = StateF[S,α]})#λ, S](Get(s =>
    Done[({type λ[+α] = StateF[S,α]})#λ, S](s)))
  def setState[S](s: S): FreeState[S,Unit] = More[({type λ[+α] = StateF[S,α]})#λ, Unit](Put(s,
    Done[({type λ[+α] = StateF[S,α]})#λ, Unit](())))

  def evalS[S,A](s: S, t: FreeState[S,A]): A = t.resume match {
    case Left(Get(f)) => evalS(s, f(s))
    case Left(Put(n, a)) => evalS(n, a)
    case Right(a) => a
  }
  def zipIndex[A](as: List[A]): List[(Int,A)] = evalS(0, as.foldLeft(
    pureState[Int, List[(Int,A)]](List())) { (acc, a) => for {
    xs <- acc
    n <- getState
    _ <- setState(n + 1)
  } yield (n,a)::xs}).reverse

  def main(args: Array[String]) {
    zipIndex((0 to 10000).toList)
  }
}
