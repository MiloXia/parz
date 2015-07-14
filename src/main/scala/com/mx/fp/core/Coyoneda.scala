package com.mx.fp.core

trait Yoneda[F[_],A] { yo =>
  def apply[B](f: A => B): F[B]
  def run: F[A] = apply(a => a)  //无需Functor实例就可以将Yoneda转变成F[A]
  def toCoyoneda: Coyoneda[F,A] = new Coyoneda[F,A] { //转Coyoneda无需Functor
    type I = A
      def fi = yo.run
      def k(i: A) = i
    }
  def map[B](f: A => B): Yoneda[F,B] = new Yoneda[F,B] { //纯粹的函数组合 map fusion
  def apply[C](g: B => C): F[C] = yo( f andThen g)
  }
}
trait Coyoneda[F[_],A] { coyo =>
  type I
  def fi: F[I]
  def k(i: I): A
  def run(implicit F: Functor[F]): F[A] =  //Coyoneda转F需要F Functor实例
    F.map(fi)(k)
  def toYoneda(implicit F: Functor[F]): Yoneda[F,A] = new Yoneda[F,A] { //转Yoneda需要Functor
  def apply[B](f: A => B): F[B] = F.map(fi)(k _ andThen f)
  }
  def map[B](f: A => B): Coyoneda[F,B] = new Coyoneda[F,B] {
    type I = coyo.I
    def fi = coyo.fi
    def k(i: I) = f(coyo k i)
  }
}
object Yoneda {
  def apply[F[_]: Functor,A](fa: F[A]) = new Yoneda[F,A] { //F转Yoneda需要Functor
  def apply[B](f: A => B): F[B] = implicitly[Functor[F]].map(fa)(f)
  }
  implicit def yonedaFunctor[F[_]] = new Functor[({type l[x] = Yoneda[F,x]})#l] {
    def map[A,B](ya: Yoneda[F,A])(f: A => B) = ya map f

  }
}
object Coyoneda {
  def apply[F[_],A](fa: F[A]): Coyoneda[F,A] = new Coyoneda[F,A] {
    type I = A          //把F[A]升格成Coyoneda, F无须为Functor
    def fi = fa
    def k(a: A) = a
  }
  implicit def coyonedaFunctor[F[_]] = new Functor[({type l[x] = Coyoneda[F,x]})#l] {
    def map[A,B](ca: Coyoneda[F,A])(f: A => B) = ca map f   //Coyoneda本身就是Functor
  }
}