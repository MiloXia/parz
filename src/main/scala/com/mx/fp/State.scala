package com.mx.fp

case class State[S,+A](run: S => (A, S)) {
  import State._
  def flatMap[B](f: A => State[S,B]): State[S,B] = State[S,B] {
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  }
  def map[B](f: A => B): State[S,B] = {
    flatMap { a => unit(f(a)) }
    //      s => {
    //        val (a, s1) = run(s)
    //        (f(a),s1)
    //      }
  }
  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] = {
    //flatMap {a => sb.map { b => f(a,b) }}
    for {
      a <- this
      b <- sb
    } yield f(a,b)
  }
  def map3[B,C,D](sb: State[S,B], sc: State[S,C])(f: (A,B,C) => D): State[S,D] ={
    for {
      a <- this
      b <- sb
      c <- sc
    } yield f(a,b,c)
  }
}
object State {
  def unit[S,A](a: A) = State[S,A](s => (a, s))
}

object StateTest extends App {
  type Stack = List[Int]
  def pop = State[Stack, Int]{ case x :: xs => (x, xs) }
  def push(i: Int) = State[Stack, Unit]{ case xs => ((), i :: xs ) }
  def stackRun: State[Stack, Int] = {
    for {
      _ <- push(13)
      a <- pop
      b <- pop
    } yield a + b
  }
  val (a, s) = stackRun.run(List(10,11,12))
  println(a,s)
  case class Acc(bar: Int)
  object Acc {
    def inc(i : Int) = State[Acc, Int]{s => (s.bar, new Acc(s.bar + i))}
    def cut(i : Int) = State[Acc, Int]{s => (s.bar, new Acc(s.bar - i))}
  }
  println(Acc.inc(2).run(Acc(1)))
  println((for {
    i <- Acc.inc(2)
    j <- Acc.cut(1)
  } yield {println(i,j); i + j}) run Acc(0))
  //more batter
  implicit class AccOp(acc :Acc) {
    def inc(i: Int) = Acc.inc(i).run(acc)._2
    def cut(i: Int) = Acc.cut(i).run(acc)._2
  }
  println(Acc(0).inc(2).cut(1))
}
