package com.mx.fp.core

case class OptionT[M[_] : Monad,A](run: M[Option[A]]) {
  def map[B](f: A => B): OptionT[M,B] = {
    val m = implicitly[Functor[M]]
    OptionT[M,B](m.map(run)(a => a.map(f)))
  }
  def flatMap[B](f: A => OptionT[M,B]): OptionT[M,B] = {
    val m = implicitly[Monad[M]]
    OptionT[M,B](m.flatMap(run) {
      case Some(a) => f(a).run
      case None => m.unit(None)
    })
  }
}
// 编译不过的 Some[List[]] 无法组合
//  val c = for {
//    a <- Some(1)
//    b <- List(1,2,3)
//  } yield a + b
//  println(c)

//val c = for {
//  a <- OptionT[List, Int](List(Some(1)))
//  b <- OptionT[List, Int](List(Some(1), Some(2), Some(3)))
//  } yield a + b
//println(c)