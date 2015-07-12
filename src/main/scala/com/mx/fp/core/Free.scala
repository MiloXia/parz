package com.mx.fp.core

import com.mx.fp.State

trait ~>[F[_],G[_]]{
  def apply[A](fa: F[A]): G[A]
}
trait Free[F[_],A] {
  private case class FlatMap[B](a: Free[F,A], f: A => Free[F,B]) extends Free[F,B]
  def unit(a: A): Free[F,A] = Return(a)
  def flatMap[B](f: A => Free[F,B])(implicit F: Functor[F]): Free[F,B] = this match {
    case Return(a) => f(a)
    case Suspend(k) => Suspend(F.map(k)(a => a flatMap f))
    case FlatMap(b,g) => FlatMap(b, g andThen (_ flatMap f))
  }

  def map[B](f: A => B)(implicit F: Functor[F]): Free[F,B] = flatMap(a => Return(f(a)))
  def resume(implicit F: Functor[F]): Either[F[Free[F,A]],A] = this match {
    case Return(a) => Right(a)
    case Suspend(k) => Left(k)
    case FlatMap(a,f) => a match {
      case Return(b) => f(b).resume
      case Suspend(k) => Left(F.map(k)(_ flatMap f))
      case FlatMap(b,g) => FlatMap(b, g andThen (_ flatMap f)).resume
    }
  }
  def foldMap[G[_]](f: (F ~> G))(implicit F: Functor[F], G: Monad[G]): G[A] = resume match {
    case Right(a) => G.unit(a)
    case Left(k) => G.flatMap(f(k))(_ foldMap f)
  }
}
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](ffa: F[Free[F,A]]) extends Free[F,A]
object Free {
  def liftF[F[_],A](fa: F[A])(implicit F: Functor[F]): Free[F,A] =
    Suspend(F.map(fa)(Return(_)))

}


object FreeTest extends App {
//  trait Interact[A]
//  case class Ask[A](prompt: String, next: A) extends Interact[A]
//  case class Tell[A](msg: String, next: A) extends Interact[A]
//  implicit val interactFunctor = new Functor[Interact] {
//    def map[A,B](ia: Interact[A])(f: A => B): Interact[B] = ia match {
//      case Ask(x,n) => Ask(x,f(n))
//      case Tell(x,n) => Tell(x,f(n))
//    }
//  }
//  implicit def LiftInteract[A](ia: Interact[A]): Free[Interact,A] = Free.liftF(ia)
//  val prg = for {
//    first <- Ask("What's your first name?",())
//    last <- Ask("What's your last name?",())
//    _ <- Tell(s"Hello $first $last",())
//  } yield ()
//  type Id[A] = A
//  implicit val idMonad: Monad[Id] = new Monad[Id] {
//    def unit[A](a: A) = a
//    def flatMap[A,B](fa: A)(f: A => B): B = f(fa)
//  }
//  object Console extends (Interact ~> Id) {
//    def apply[A](i: Interact[A]): A = i match {
//      case Ask(prompt, n) => {
//        println(prompt)
//        readLine; n
//      }
//      case Tell(msg, n) => println(msg); n
//    }
//  }
//  prg.foldMap(Console)
  /////
  trait Console[A]
  case class GetLine[A](next: A) extends Console[A]
  case class PutLine[A](msg: String, next: A) extends Console[A]
  implicit val consoleFunctor = new Functor[Console]{
    def map[A,B](ca: Console[A])(f: A => B): Console[B] = ca match {
      case GetLine(a) => GetLine(f(a))
      case PutLine(m,a) => PutLine(m,f(a))
    }
  }
  type ConsoleIO[A] = Free[Console,A]
  implicit def liftConsole[A](ca: Console[A]) = Free.liftF(ca)
  def putLine(msg: String) = PutLine(msg,())
  def getLine = GetLine(())
  val ioprg:ConsoleIO[Unit] = for {
      _ <- putLine("What is your first name ?")
      first <- getLine
      _ <- putLine("What is your last name ?")
      last <- getLine
      _ <- putLine(s"Hello, $first $last !")
  } yield()
  type Id[A] = A
  implicit val idMonad = new Monad[Id] {
    def unit[A](a: A): A = a
    def flatMap[A,B](fa: A)(f: A => B): B = f(fa)
  }
  object ConsoleEffect extends (Console ~> Id) {
    def apply[A](c: Console[A]): A = c match {
      case GetLine(n) =>  readLine; n
      case PutLine(m,n) => println(m); n
    }
  }
  ioprg.foldMap(ConsoleEffect)

//  trait StateF[S,A]
//  case class Get[S,A](f: S => A) extends StateF[S,A]
//  case class Put[S,A](s: S, a: A) extends StateF[S,A]
//  implicit def stateFFunctor[S] = new Functor[({type l[x] = StateF[S,x]})#l] {
//    def map[A,B](sa: StateF[S,A])(f: A => B): StateF[S,B] = sa match {
//      case Get(g) => Get( s => f(g(s)) )
//      case Put(s,a) => Put(s, f(a))
//    }
//  }
//  type FreeState[S,A] = Free[({type l[x] = StateF[S,x]})#l, A]
//  def unit[S,A](a: A): FreeState[S,A] = Return[({type l[x] = StateF[S,x]})#l, A](a)
//  def getState[S]: FreeState[S,S] = Suspend[({type l[x] = StateF[S,x]})#l, S](
//    Get(s => Return[({type l[x] = StateF[S,x]})#l, S](s)))
//  def setState[S](s: S): FreeState[S,Unit]  = Suspend[({type l[x] = StateF[S,x]})#l, Unit](
//    Put(s, Return[({type l[x] = StateF[S,x]})#l, Unit](())))
//  def evalS[S,A](s: S, t: FreeState[S,A]): A = t.resume match {
//    case Right(a) => a
//    case Left(Get(f)) => evalS(s, f(s))
//    case Left(Put(n,a)) => evalS(n,a)
//  }
//  def zipIndex[A](as: List[A]): List[(Int, A)] = {
//    evalS(1, as.foldLeft(unit[Int,List[(Int,A)]](List()))(
//      (acc,a) => for {
//        xs <- acc
//        n <- getState
//        _ <- setState(n+1)
//      } yield (n, a) :: xs)).reverse
//  }
//  println(zipIndex((0 to 10000).toList))
  trait StackOps[A]
  case class Push[A](value: Int, ops:A) extends StackOps[A]
  case class Add[A](ops: A) extends StackOps[A]
  implicit val stackOpsFunctor: Functor[StackOps] = new Functor[StackOps] {
    def map[A,B](oa: StackOps[A])(f: A => B): StackOps[B] = oa match {
      case Push(v,a) => Push(v,f(a))
      case Add(a) => Add(f(a))
    }
  }
  def liftF[F[_],A](fa: F[A])(implicit F: Functor[F]): Free[F,A] = {
    Suspend(F.map(fa)(a => Return(a)))
  }
  implicit def liftStackOps[A](sa: StackOps[A]): Free[StackOps,A] = liftF(sa)
  val stkprg = for {
    _ <- Push(1,())
    _ <- Push(2,())
    _ <- Add(())
    _ <- Push(3,())
  } yield ()
  type Stack = List[Int]
  type StackState[A] = State[Stack,A]
  implicit val stackStateMonad = new Monad[StackState] {
    def unit[A](a: A) = State(s => (a,s))
    def flatMap[A,B](sa: StackState[A])(f: A => StackState[B]): StackState[B] = sa flatMap f
  }
  object StackOperator extends (StackOps ~> StackState) {
    def apply[A](sa: StackOps[A]): StackState[A] = sa match {
      case Push(v,n) => State((s: Stack) => (n, v :: s))
      case Add(n) => State((s: Stack) => {
        val hf :: hs :: t = s
        (n, (hf + hs) :: s)
      })
    }
  }
  println(stkprg.foldMap(StackOperator).run(List[Int]()))
}