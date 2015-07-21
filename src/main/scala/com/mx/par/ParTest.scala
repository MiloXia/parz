package com.mx.par

import com.mx.fp.core.Monoid
import com.mx.fp.core.Monad

/**
 * Created by milo on 15-7-21.
 */
object ParTest extends App {

  trait Free[F[_],A] {
    def unit(a: A): Free[F,A] = Done(a)
    def flatMap[B](k: A => Free[F,B]): Free[F,B] = this match {
      case Done(a) =>
        k(a)
      case Blocked(reqs, cont) =>
        Blocked(reqs, cont andThen (_ flatMap k))
    }
    def map[B](f: A => B): Free[F,B] =
      flatMap(f andThen (Done(_)))
    //app
    def map2[B, C](mb: Free[F, B])(f: (A, B) => C)(implicit M: Monoid[F[_]]): Free[F, C] = (this, mb) match {
      case (Done(a), Done(b)) =>
        Done(f(a, b))
      case (Done(a1), Blocked(reqs, cont)) =>
        Blocked(reqs, (x: Any) => Done(a1).map2(cont(x))(f))
      case (Blocked(reqs, cont), Done(b1)) =>
        Blocked(reqs, (x: Any) => cont(x).map2(Done(b1))(f))
      case (Blocked(reqs1, cont1), Blocked(reqs2, cont2)) =>
        Blocked(M.op(reqs1, reqs2), (x: Any) => cont1(x).map2(cont2(x))(f))
    }

    def apply[B](fab: Free[F, A => B])(implicit M: Monoid[F[_]]): Free[F, B] = { //<*>
      this.map2(fab)((a, f) => f(a))
    }

    def foldMap[G[_]: Monad](f: F ~> G): G[A] = {
      val G = implicitly[Monad[G]]
      this match {
        case Done(a) =>
          G.unit(a)
        case Blocked(reqs, cont) =>
          G.flatMap(f(reqs))(cont andThen (_ foldMap f))
      }
    }
  }

  final case class Done[F[_], A](a: A) extends Free[F,A]
  final case class Blocked[F[_], R, A](reqs: F[R], cont: R => Free[F,A]) extends Free[F,A]

  trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  object Free {
    def lift[F[_],R](reqs: F[R]): Free[F,R] =
      Blocked(reqs, (x:R) => Done(x))
  }


//  type Requests = List[Request[_]]
//  trait Responses
  trait Request[A]
  case class Breakfast(egg: Int, milk: Int)
  case object GetUp extends Request[Unit]
  case object Wash extends Request[Unit]
  case object MakeBreakfast extends Request[Breakfast]
  case class Eat(breakfast: Breakfast) extends Request[Unit]
  case object GoOut extends Request[Unit]
  case object TakeBus extends Request[Unit]

  case class Something(l: List[Request[_]]) extends Request[Unit]
  case object Nonthing extends Request[Nothing]
  //test
  case class Get(q: String) extends Request[String]
  case class Put(r: String) extends Request[Unit]

  type FreeRequest[A] = Free[Request, A]
  implicit def lift[A](n: Request[A]): FreeRequest[A] = Free.lift(n)

  implicit def reqMonoid[_]: Monoid[Request[_]] = new Monoid[Request[_]] {
    def op(req1: Request[_], req2: Request[_]): Request[_] = (req1, req2) match {
      case (Something(l1), Something(l2)) => Something(l1 ++ l2)
      case (Something(l), r) => Something(r :: l)
      case (r, Something(l)) => Something(r :: l)
      case (r1, r2) => Something(r1 :: r2 :: Nil)
    }
    val zero: Request[_] = Something(Nil)
  }
  val early: FreeRequest[Unit] = for {
    _ <- GetUp
    _ <- Wash
    breakfast <- MakeBreakfast
    _ <- Eat(breakfast)
    _ <- GoOut
    _ <- TakeBus
  } yield ()

  val late: FreeRequest[Unit] = for {
    _ <- GetUp
    _ <- Wash
    breakfast <- MakeBreakfast
    _ <- Eat(breakfast).map2(GoOut)((_:Unit, _:Unit) => ())
    _ <- TakeBus
  } yield ()

  val doo1 = Blocked(GetUp, (_:Unit) =>
    Blocked(Wash, (_:Unit) =>
      Blocked(MakeBreakfast, (breakfast: Breakfast) =>
        Blocked(Eat(breakfast), (_: Unit) =>
          Blocked(GoOut, (_:Unit) =>
            Blocked(TakeBus, (_: Unit) =>
              Done(()))))))) : Free[Request, Unit]

  val x1 = Blocked(GetUp, (_:Unit) =>
    Blocked(Wash, (_:Unit) =>
      Blocked(MakeBreakfast, (breakfast: Breakfast) =>
        Blocked(Eat(breakfast), (_: Unit) =>
          Done(()))))) : Free[Request, Unit]
  val x2: Free[Request, Unit] = Blocked(GoOut, (_:Unit) => Done(()))
  val x3: Free[Request, Unit] = x1.map2(x2)((_:Unit, _:Unit) => ()) //GetUp will do in parallel with GoOut
  val doo2: Free[Request, Unit] = x3.flatMap((_:Unit) => Blocked(TakeBus, (_: Unit) => Done(())))


  val test = for {
    a <- Get("get a")
    d <- Get("get b").map2(Get("get c"))((b: String, c: String) => b + c)
    _ <- Put(a+d)
  } yield ()

  type Id[A] = A
  implicit val IdMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: A) = a
    def flatMap[A,B](m: Id[A])(k: A => Id[B]): Id[B] =
      m match { case a => k(a) }
  }

  object ConsoleEffect extends (Request ~> Id) {
    def apply[A](nl: Request[A]): Id[A] = nl match {
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
      case Something(l) =>
        println(s"Do [${l.mkString(",")}] in parallel")
      case Get(q) =>
        println(q)
        readLine
      case Put(s) =>
        println(s)
    }
  }

  early.foldMap(ConsoleEffect)
  println("---------------------")
  late.foldMap(ConsoleEffect)
  println("---------------------")
  doo1.foldMap(ConsoleEffect)
  println("---------------------")
  doo2.foldMap(ConsoleEffect)
  println("---------------------")
  test.foldMap(ConsoleEffect)
}
