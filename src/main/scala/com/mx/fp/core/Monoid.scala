package com.mx.fp.core

trait Monoid[A] {
  def op(a1: A, a2: A): A
  val zero: A
}

