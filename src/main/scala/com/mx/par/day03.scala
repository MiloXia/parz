package com.mx.par

import scala.collection.immutable.Map

import com.mx.fp.State
import com.mx.fp.core.{Monad, Monoid}

import scala.util.Random

/**
 * Created by milo on 15-7-23.
 */
object day03 extends App {

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
    //Applicative
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

  case class Responses(store: Map[Request[_], Any])
  object Responses {//state
    def fetch[A](req: Request[A], resp: Responses): A = State[Responses, A] { s =>
      (s.store(req).asInstanceOf[A], s)
    }.run(resp)._1
    def add(req: Request[_], value: Any): Responses = State[Responses, Unit] { s =>
      ((), Responses(Map[Request[_], Any](req -> value) ++ s.store))
    }.run(Monoid.zero)._2

    val Monoid = new Monoid[Responses] {
      def op(resp1: Responses, resp2: Responses) = (resp1, resp2) match {
        case (Responses(r1), Responses(r2)) => Responses(r1 ++ r2)
      }
      val zero: Responses = Responses(Map[Request[_], Any]())
    }
  }

  trait Request[A]
  case class Breakfast(egg: Int, milk: Int)
  case object GetUp extends Request[Unit]
  case object Wash extends Request[Unit]
  case object MakeBreakfast extends Request[Breakfast]
  case class Eat(breakfast: Breakfast) extends Request[Unit]
  case object GoOut extends Request[Unit]
  case object TakeBus extends Request[Unit]

  //Monoid
  case class Requests(l: List[Request[_]]) extends Request[Responses]
  object Requests {
    def add[A](req: Request[A]): Requests = Requests(req :: Nil)
    def empty = Requests(Nil)
  }
  //test
  case class Get(q: String) extends Request[String]
  case class Put(r: String) extends Request[Unit]

  type FreeRequest[A] = Free[Request, A]
  implicit def lift[A](req: Request[A]): FreeRequest[A] =
    Blocked(Requests add req, (resp: Responses) => Done(Responses.fetch(req, resp)))

  implicit def reqMonoid[_]: Monoid[Request[_]] = new Monoid[Request[_]] {
    def op(req1: Request[_], req2: Request[_]): Request[_] = (req1, req2) match {
      case (Requests(l1), Requests(l2)) => Requests(l1 ++ l2)
      case other => throw new Exception(s"bad request $other")
//      case (Requests(l), r) => Requests(r :: l) //Impossible
//      case (r, Requests(l)) => Requests(r :: l) //Impossible
//      case (r1, r2) => Requests(r1 :: r2 :: Nil)//Impossible
    }
    val zero: Request[_] = Requests.empty
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

  object FetchEffect extends (Request ~> Id) { //F[A] is Requests[Responses]
    def apply[A](nl: Request[A]): Id[A] = nl match {
      case Requests(l) =>
        if(l.length > 1) println(s"Do [${l.mkString(",")}] in parallel")
        fetch(l)
      case _ => throw new Exception("bad cmd")
    }

    private def translate[A](nl: Request[A]): Id[A] = nl match {
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
      case Get(q) =>
        println(q)
        Random.nextString(10)
      case Put(s) =>
        println(s)
    }

    def fetch(l: List[Request[_]]): Responses = {
      l.par.map{ r =>
        println(s"--${Thread.currentThread().getName}")
        Responses.add(r, translate(r))
      }.fold(Responses.Monoid.zero)(Responses.Monoid.op)
    }
  }
  early.foldMap(FetchEffect)
  println("-" * 20)
  late.foldMap(FetchEffect)
  println("-" * 20)
  test.foldMap(FetchEffect)
}
