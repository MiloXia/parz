package com.mx.fp.core

trait Trampoline[+A] {
  final def runT: A = resume match {
    case Right(a) => a
    case Left(k) => k().runT
  }
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = {
    this match {
      //          case Done(a) => f(a)
      //          case More(k) => f(runT)
      case FlatMap(a,g) => FlatMap(a, (x: Any) => g(x) flatMap f)
      case x => FlatMap(x, f)
    }
  }
  def map[B](f: A => B) = flatMap(a => Done(f(a)))
  def resume: Either[() => Trampoline[A], A] = this match {
    case Done(a) => Right(a)
    case More(k) => Left(k)
    case FlatMap(a,f) => a match {
      case Done(v) => f(v).resume
      case More(k) => Left(() => k() flatMap f)
      case FlatMap(b,g) => FlatMap(b, (x: Any) => g(x) flatMap f).resume
    }
  }
}
case class Done[+A](a: A) extends Trampoline[A]
case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
case class FlatMap[A,B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]

//object Test extends App {
//  def foldRight[A, B](l :List[A], z: B)(op: (A, B) => B): B = l match {
//    case Nil => z
//    case h :: t => op(h, foldRight(t, z)(op))
//  }
//  //  println(foldRight((1 to 10000).toList, 0)(_+_)) //stackoverflow
//  def foldRight2[A, B](l :List[A], z: B)(op: (A, B) => B): Trampoline[B] = l match {
//    case Nil => Done(z)
//    case h :: t => More(() => {
//      foldRight2(t, z)(op) flatMap {
//        a => More(() => Done(op(h, a)))
//      }
//    })
//  }
//  println(foldRight2((1 to 10000).toList, 0)((a,b) => {println(a,b);a+b}).runT)
//}
