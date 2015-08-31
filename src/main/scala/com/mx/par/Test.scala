package com.mx.par

import scala.util.Random

import com.mx.fp.State
import com.mx.fp.core.{Monoid, Monad}
import com.mx.par.day05.{Blocked, Free, Done}
import com.mx.par.day07.Request
import com.mx.par.day07.{Get, Put}
import shapeless.PolyDefns.~>

/**
 * Created by milo on 15-7-20.
 */
object Test extends App {
  case class Breakfast(egg: Int, milk: Int)
  sealed trait GoToWork[A]
  case object GetUp extends GoToWork[Unit]
  case object Wash extends GoToWork[Unit]
  case object MakeBreakfast extends GoToWork[Breakfast]
  case class Eat(breakfast: Breakfast) extends GoToWork[Unit]
  case object GoOut extends GoToWork[Unit]

  trait DoFree[F[_],A] {
    def unit(a: A): DoFree[F,A] = Return(a)
    def flatMap[B](k: A => DoFree[F,B]): DoFree[F,B] = this match {
      case Return(a) =>
        k(a)
      case Bind(command, cont) =>
        Bind(command, cont andThen (_ flatMap k))
    }
    def map[B](f: A => B): DoFree[F,B] =
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
  final case class Return[F[_], A](a: A) extends DoFree[F,A]
  final case class Bind[F[_], R, A](command: F[R],
                                    cont: R => DoFree[F,A]) extends DoFree[F,A]

  trait ~>[F[_],G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  object DoFree {
    def lift[F[_],R](command: F[R]): DoFree[F,R] =
      Bind(command, (x:R) => Return(x))
  }

  type FreeGoToWork[A] = DoFree[GoToWork, A]
  implicit def liftGoToWork[A](n: GoToWork[A]): FreeGoToWork[A] = DoFree.lift(n)

  val doo: FreeGoToWork[Unit] = for {
    _ <- GetUp
    _ <- Wash
    breakfast <- MakeBreakfast
    _ <- Eat(breakfast)
    _ <- GoOut
  } yield ()

  Bind(GetUp, (_:Unit) =>
    Bind(Wash, (_:Unit) =>
      Bind(MakeBreakfast, (breakfast: Breakfast) =>
        Bind(Eat(breakfast), (_: Unit) =>
          Bind(GoOut, (_:Unit) =>
            Return(())))))) : DoFree[GoToWork,Unit]

  type Id[A] = A
  implicit val IdMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: A) = a
    def flatMap[A,B](m: Id[A])(k: A => Id[B]): Id[B] =
      m match { case a => k(a) }
  }

  object ConsoleEffect extends (GoToWork ~> Id) {
    def apply[A](nl: GoToWork[A]): Id[A] = nl match {
      case GetUp =>
        println("get up")
      case Wash =>
        println("wash")
      case MakeBreakfast =>
        Breakfast(2, 1)
      case Eat(b) =>
        println(s"eat $b")
      case GoOut =>
        println(s"go out")
    }
  }

  doo.foldMap(ConsoleEffect)
}

object TestForAll extends App {
  trait Show[T] {
    def apply(t:T): String
  }
  implicit object ShowInt extends Show[Int] {
    def apply(t:Int) = "Int("+t+")"
  }
  implicit object ShowBoolean extends Show[Boolean] {
    def apply(t:Boolean) = "Boolean("+t+")"
  }

  case class ShowBox[T: Show](t:T) {
    def show = implicitly[Show[T]].apply(t)
  }

  implicit def box[T: Show]( t: T ) =
    new ShowBox(t)

  val lst: List[ShowBox[_]] = List( 2, true )

  println( lst )

  val lst2 = lst.map( _.show )

  println( lst2 )
}

object HListTest extends App {
  import shapeless._

  val l1 = 2 :: "a" :: HNil
  val l2 = "b" :: 3 :: HNil
  val h = l1.head
  val h1 :: h2 :: HNil = l1
  val l3 = l1 ++ l2
  val x1 :: x2 :: x3 :: rest = l3
  val reqs = Get("s") :: Put("s") :: HNil
  //Id
//  implicit object mapper extends (({ type O2[+A] = Option[A] })#O2 ~> Option) {
//    def apply[A](x: Option[A]): Option[A] = x match {
//      case Some(s) => println(s); Some(s)
//      case _ => None
//    }
//  }
//  val l = Option(1) :: Option(2) :: None :: Option(4) :: HNil
//  l.map(mapper)
//  case class Responses[F[_]](store: Map[F[_], Any])
//  object Responses {//state
//    def fetch[F[_], A](req: F[A], resp: Responses[F]): A = State[Responses[F], A] { s =>
//      (s.store(req).asInstanceOf[A], s)
//    }.run(resp)._1
//
//    def add[F[_]](req: F[_], value: Any): Responses[F] = State[Responses[F], Unit] { s =>
//      ((), Responses[F](Map[F[_], Any](req -> value) ++ s.store))
//    }.run(Monoid[F].zero)._2
//
//    implicit def Monoid[F[_]]: Monoid[Responses[F]] = new Monoid[Responses[F]] {
//      def op(resp1: Responses[F], resp2: Responses[F]) = (resp1, resp2) match {
//        case (Responses(r1), Responses(r2)) => Responses[F](r1 ++ r2)
//      }
//      val zero: Responses[F] = Responses[F](Map[F[_], Any]())
//    }
//  }
//
//  val resp1 = Responses[List](Map(List(1) -> 1))
//  val resp2 = Responses.add(List(2), 2)
//  val resp3 = Responses.Monoid[List].op(resp1, resp2)
//  val x = Responses.fetch(List(1), resp3)
//  println(resp1)
//  println(resp2)
//  println(resp3)
//  println(x)

//  trait Request[A]
//  case class Breakfast(egg: Int, milk: Int)
//  case object GetUp extends Request[Unit]
//  case object MakeBreakfast extends Request[Breakfast]
//
//  //Monoid
//  case class Requests[F[_]](l: List[F[_]]) extends F[Responses[F]]
//  object Requests {
//    def add[F[_], A](req: F[A]): Requests[F] = Requests[F](req :: Nil)
//    def empty = Requests(Nil)
//  }
//  implicit def reqMonoid[F[_]]: Monoid[F[_]] = new Monoid[F[_]] {
//    def op(req1: F[_], req2: F[_]): F[_] = (req1, req2) match {
//      case (Requests(l1), Requests(l2)) => Requests[F](l1 ++ l2)
//      case other => throw new Exception(s"bad request $other")
//      //      case (Requests(l), r) => Requests(r :: l) //Impossible
//      //      case (r, Requests(l)) => Requests(r :: l) //Impossible
//      //      case (r1, r2) => Requests(r1 :: r2 :: Nil)//Impossible
//    }
//    val zero: Request[_] = Requests.empty
//  }
//
//  implicit def dataFetch[F[_], A](req: F[A]): Free[F, A] =
//    Blocked(Requests add req, (resp: Responses[Request]) => Done(Responses.fetch(req, resp)))
//
//
//  val reqs = Requests.add(List(1))
//  def dataFetch[F[_], A](reqs: F[Responses[F]])(req: F[A]): Free[F, A] = {
//    Blocked(reqs, (resp: Responses[F]) => Done(Responses.fetch(req, resp)))
//  }
//
//  implicit def lift[F[_], A](req: F[A]) =  dataFetch(Requests.add(req))(req)
//
//  trait ~>[F[_], G[_]] {
//    def apply[A](fa: F[A]): G[A]
//  }
//  type Id[A] = A
//  def FetchEffect[F[_]] = new (Req ~> Id) { //F[A] is Req[Responses[F]]
//    def apply[A](nl: Req[A]): Id[A] = nl match {
//      case Requests(l) =>
//        if(l.length > 1) println(s"Do [${l.mkString(",")}] in parallel")
//        fetch(l)
//      case _ => throw new Exception("bad cmd")
//    }
//
//    def fetch(l: List[F[_]]): Responses[F] = {
//      l.par.map{ r =>
//        println(s"--${Thread.currentThread().getName}")
//        Service.fetch(r)
//      }.fold(Responses.Monoid.zero)(Responses.Monoid.op)
//    }
//  }
//  //service
//  trait Service[F[_]] {
//    def deal[A](req: F[A]): Responses[F]
//  }
//
//  object Service {
//    implicit object DataService extends Service[Request] {
//      def deal[A](req: Request[A]): Responses[Request] = req match {
//        case GetUp =>
//          Responses.add(req, println("get up"))
//        case MakeBreakfast =>
//          println("make breakfast")
//          Responses.add(req, Breakfast(2, 1))
//      }
//    }
//    def fetch[R[_]: Service, A](req: R[A]) = {
//      implicitly[Service[R]].deal(req)
//    }
//  }
}

object PFTest extends App {
  def mkPF(i: Int) = {
    new PartialFunction[Int, Int] {
      def isDefinedAt(i: Int) = {true}
      def apply(i: Int) = {Thread.sleep(100) ; i}
    }
  }
  val start = System.nanoTime()
  var x1: PartialFunction[Int, Int] = mkPF(0)
  val f = {
    for(i <- (1 to 100000)) {x1 = x1 orElse mkPF(i)}
    x1
  }
  val end = System.nanoTime()
  println( (end - start) / 1000000 )

  val start1 = System.nanoTime()
  println(f(9999))
  val end1 = System.nanoTime()
  println( (end1 - start1) / 1000000)
}

object TTest extends App {
  trait A[A]
  case class AA(a: Int) extends A[Int]
  case class BB(b: String) extends A[String]

  trait Hashable[T[_]] {
    def hash[X](x: T[X]) = x.hashCode
  }

  object Hashable {
    def hash[T[_]: Hashable, X](x:T[X]) = {
      implicitly[Hashable[T]].hash(x)
    }
//    implicit object cmpA extends Hashable[A] {
//      override def hash[X](x: A[X]) = super.hash(x)
//    }
  }

  object A {
    implicit object cmpA extends Hashable[A] {
      override def hash[X](x: A[X]) = super.hash(x)
    }
  }

  object AAA {
    def run: Unit = {
      val x = AA(1)
      val y = AA(2)
      val z = BB("a")
      val l: List[A[_]] = List(x, y, z)
      l.foreach { a =>
        println(Hashable.hash(a))
      }
    }
  }

  AAA.run
//  val x2 = new B(1)
//  val y2 = new B(2)
//  println(cmp(x2, y2) > 0)
}

object TTa extends App {
//  def mkPF[T] = {
//    new PartialFunction[T, Int] {
//      def isDefinedAt(i: T) = true
//      def apply(i: T) = {1}
//    }
//  }
//  List(1,2,3).map(_+1)
//  List(1,2,3).map(mkPF)
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  val f1 = future { 1 }
  val f2 = future { 2 }
  val f3 = for {
    a <- f1
    b <- f2
  } yield a + b

  def f(x: Int) = Some(x)
  def g(y: String) = Some(y)
  def fg(x: Int, y: String) = for {
    a <- f(x)
    b <- g(y)
  } yield a + b
}

