package com.mx.par.core.test

import java.lang.UnsupportedOperationException

/**
 * Created by milo on 15-8-28.
 */

class HRequest[+Out](o:Out) {
  type O = Out

  def v:O = o
}

object HList {
  trait Func[-Elem,Out] {
    type Apply[E <: Elem] <: Out
    def apply[N <: Elem](e:N):Apply[N]
  }
  sealed trait HList[Base] {
    type Head <: Base
    type Tail <: HList[Base]
    type Map[Out,F <: Func[Base,Out]] <: HList[Out]
    def head:Head
    def tail:Tail

    def ::[A <: Base](a:A):HList[Base]
    def map[Out,F <: Func[Base,Out]](f:F):Map[Out,F]
  }

  case class HNil[Base]() extends HList[Base] {
    type Head = Nothing
    type Tail = Nothing
    type Map[Out,F <: Func[Base,Out]] = HNil[Out]

    def head = throw new UnsupportedOperationException("Head of an empty HList")
    def tail = throw new UnsupportedOperationException("Head of an empty HList")

    def ::[A <: Base](a:A) = HCons(a,this)
    def map[Out,F <: Func[Base,Out]](f:F) = new HNil[Out]
  }

  case class HCons[Base,A <: Base,B <: HList[Base]](head: A, tail: B) extends HList[Base] {
    type Head = A
    type Tail = B
    type Map[Out,F <: Func[Base,Out]] = HCons[Out,F#Apply[Head],Tail#Map[Out,F]]

    def ::[C <: Base](c:C) = HCons(c,this)
    def map[Out,F <: Func[Base,Out]](f:F) =
      HCons(f(head),tail.map(f))
  }

  val :: = HCons
}

object Test extends App {
  import com.mx.par.core.test.HList._

  val HNil = new HNil[HRequest[_]]

  val list = new HRequest[Int](1) :: new HRequest[String]("1") :: HNil

  val (a :: b :: HNil) = list
  println(a)
  val y:HRequest[String] = b

  val results = list.map[Any,Unwrap.type](Unwrap)
  println(results)

  val i:Int = results.head.asInstanceOf[Int]
  println(i)
}

import com.mx.par.core.test.HList._
object Unwrap extends Func[HRequest[Any],Any] {
  type Apply[I <: HRequest[Any]] = I#O
  def apply[N <: HRequest[Any]](e:N) = e.v//.asInstanceOf[Apply[N]]
}

