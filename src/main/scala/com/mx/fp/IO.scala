package com.mx.fp

import com.mx.fp.core.Monad

trait IO[+A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] =
    new IO[B] { def run = f(self.run)}
  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {def run = f(self.run).run}
}

object IO extends Monad[IO] {
  def unit[A](a: A) = new IO[A] {def run = a}
  def flatMap[A,B](ma: IO[A])(f: A => IO[B]) = ma flatMap f
  override def map[A,B](ma: IO[A])(f: A => B) = ma map f
  def apply[A](a: A) = unit(a)
}

object FreeIO {

  // what is a free monad?

  /** `F` is _usually_ some sealed trait with a fixed list of
    * I/O operations encoded as an enum. The tparam that it takes
    * is the result type of each I/O operation.
    *
    * `sealed` means that all possible subclasses are in this file.
    * Useful for creating enums.
    *
    * Request and Return are used to chain I/O operations together.
    * Return is just a plain wrapper around the value, waiting to receive
    * and pass along whatever function it gets through #flatMap.
    *
    * IO is a (free?) monad.
    *
    * @tparam F a higher-kinded type
    * @tparam A "wrapped" or "returned" type
    * @see http://blog.higher-order.com/assets/scalaio.pdf
    */
  sealed trait IO[OP[_], A] {
    /** Chains Requests and Returns */
    def flatMap[B](f: A => IO[OP, B]): IO[OP, B]

    def map[B](f: A => B): IO[OP, B] =
      flatMap { a => Return(f(a)) }

    /* Interestingly, it's not possible for runIO to take just a function, because
     * scala doesn't allow you to pass a generic function around i.e. runIO(func)
     * wouldn't work, because scala wouldn't know if it's func[String] or func[Unit].
     * This explains why we need Effect[F].
     *
     * To make effects even more flexible, we allow each effect to return a Monad,
     * so that additional state and be passed around e.g. ListEffect.
     */

    /** Executes the encoded request/return operations using `effect`.
      *
      * Think of `F ~> G` as `Effect[F, G]`. Just syntax sugar.
      *
      * G[_]: Monad means that
      * - we expect a higher-kinded type G, that
      * - has an associated Monad that is passed in as an implicit Monad[G].
      *
      * @see https://twitter.github.io/scala_school/advanced-types.html
      * @see http://stackoverflow.com/questions/2982276/what-is-a-context-bound-in-scala
      */
    def runIO[G[_] : Monad](effect: OP ~> G): G[A]
  }

  case class Return[OP[_], A](value: A) extends IO[OP, A] {
    override def flatMap[B](f: A => IO[OP, B]): IO[OP, B] =
      f(value)

    override def runIO[G[_] : Monad](effect: OP ~> G): G[A] = {
      // implicity[Monad[G]] returns an implicit value of type Monad[G]
      val G: Monad[G] = implicitly[Monad[G]]
      G.of(value)
    }
  }

  case class Req[OP[_], I, A](op: OP[I], cont: I => IO[OP, A]) extends IO[OP, A] {
    override def flatMap[B](f: A => IO[OP, B]): IO[OP, B] =
      Req(op, cont andThen {
        _ flatMap f
      })

    // k is like a continuation
    // Here, we compose k with f using flatMap.

    override def runIO[G[_] : Monad](effect: OP ~> G): G[A] = {
      // implicity[Monad[G]] returns an implicit value of type Monad[G]
      val G: Monad[G] = implicitly[Monad[G]]
      G.flatMap(effect(op))(cont andThen {
        _ runIO effect
      })
      // XXX The following doesn't work
      //   (k andThen { _.runIO(e)})(effect(req))
      // because effect(req) returns a Monad of A, not just A.
    }
  }

  /** Side effects for I/O.
    *
    * Note that for simple effects, such as StdEffect, #apply could just return X. However, for advanced effects such as ListEffect,
    * additional state needs to be passed around, which is why we allow G, a higher-kinded type, to be wrapped about X.
    *
    * G is a monad. I renamed it to RESULT for readability.
    */
  trait ~>[OP[_], RESULT[_]] {
    // think of this as Effect[F[_], G[_]]
    /** Applies effect on op. */
    def apply[X](op: OP[X]): RESULT[X]
  }

  /** An enum that is used to encode IO operations and their parameters for Console.
    */
  sealed trait Console[A]
  case object GetLine extends Console[String]
  case class PutLine(s: String) extends Console[Unit]

  object ConsoleIO {
    // higher-kinded type - new syntax!
    type ConsoleIO[A] = IO[Console, A]

    val getLine: ConsoleIO[String] =
    // op: Console[String]
    // cont: String => IO[Console, String]
      Req(GetLine, (s: String) => Return(s))

    def putLine(s: String): ConsoleIO[Unit] =
    // op: Console[Unit]
    // cont: Unit => IO[Console, Unit]
      Req(PutLine(s), (_: Unit) => Return(()))
  }
  /** Implements an effect for Console using standard in and standard out. */
  object StdEffect extends (Console ~> Id) {
    def apply[A](r: Console[A]): Id[A] =
      r match {
        case GetLine => readLine
        case PutLine(s) => println(s)
      }
  }

  case class InOut(in: List[String], out: List[String])
  case class ListState[A](runState: InOut => (A, InOut))

  /** Implements an effect for Console using an input list and an output list. */
  object ListEffect extends (Console ~> ListState) {
    // INSIGHT!
    //
    // For ListEffect, we need additional state to be passed around. It's no longer sufficient to return A (the line that was read); we also
    // need to pass along the state of the list - what has been read, what is leftover. For this reason, we should return a Monad of A instead
    // of A to carry along this additional state.
    def apply[A](op: Console[A]): ListState[A] =
      ListState(state => // Implemented as a function, because the list state does not get passed to apply. Thus, this effect has to be a
        // deferred effect, and the application must call runState explicitly.
        (op, state) match {
          case (GetLine, InOut(in, out)) =>
            (in.head, InOut(in.tail, out))
          case (PutLine(line), InOut(in, out)) =>
            ((), InOut(in, line::out))
        }
      )
  }

  trait Monad[M[_]] {
    def of[A](a: A): M[A]
    def flatMap[A, B](a: M[A])(f: A => M[B]): M[B]
  }

  type Id[A] = A

  implicit val MonadId: Monad[Id] = new Monad[Id] {
    def of[A](value: A): Id[A] = value
    def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] = f(value)
    override def toString = s"free.MonadId: ${getClass}"
  }
  implicit val MonadListState: Monad[ListState] = new Monad[ListState] {
    def of[A](value: A): ListState[A] =
      ListState(any => (value, any))
    def flatMap[A, B](value: ListState[A])(f: A => ListState[B]): ListState[B] =
      value match {
        case ListState(leftFunc) =>
          ListState(leftFunc andThen { case (v1, inOut1) => f(v1).runState(inOut1) })
      }
    override def toString = s"free.MonadListState: ${getClass}"
  }

  import ConsoleIO._
  def main(args: Array[String]) {
    val ask: ConsoleIO[Unit] = for {
      _ <- putLine("What is your name?") // flatmap
      name <- getLine                    // flatmap
      _ <- putLine(s"Hello ${name}")     // map
    } yield ()

    // Use stdin and stdout
    println(ask.runIO(StdEffect))

    // Use a mock IO list
    val io = ask.runIO(ListEffect)
    println(io.runState(InOut(List("Stranger"), List())))
  }
}