package com.mx.fp

object Par {
  import java.util.concurrent._
  type Par[A] = ExecutorService => Future[A]
  def run[A](pa: Par[A])(implicit es: ExecutorService): Future[A] = pa(es)
  def unit[A](a: A): Par[A] = es => {
    new Future[A] {
      def get: A = a
      def isDone = true
      def isCancelled = false
      def get(timeOut: Long, timeUnit: TimeUnit): A = get
      def cancel(evenIfRunning: Boolean): Boolean = false
    }
  }
  def fork[A](pa: => Par[A]): Par[A] = es => {
    es.submit[A](new Callable[A] {
      def call: A = run(pa)(es).get
    })
  }
  def async[A](a: => A): Par[A] = fork(unit(a))
  //applicative
  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] = {
    import TimeUnit.NANOSECONDS
    es => new Future[C] {
      val fa = run(pa)(es)
      val fb = run(pb)(es)
      def get = f(fa.get, fb.get)
      def get(timeOut: Long, timeUnit: TimeUnit) = {
        val start = System.nanoTime
        val a = fa.get
        val end = System.nanoTime
        val b = fb.get(timeOut - timeUnit.convert(end - start, NANOSECONDS) , timeUnit)
        f(a,b)
      }
      def isDone = fa.isDone && fb.isDone
      def isCancelled = fa.isCancelled && fb.isCancelled
      def cancel(evenIsRunning: Boolean) = fa.cancel(evenIsRunning) || fb.cancel(evenIsRunning)
    }
  }
  //map2 >> map3 : map3(a,b,c)(f) = map2(a, map2(b,c)(f))(f)
  def map3[A,B,C,D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A,B,C) => D): Par[D] = {
    map2(pa,map2(pb,pc){(b,c) => (b,c)}){(a,bc) => {
      val (b,c) = bc
      f(a,b,c)
    }}
  }
  //monad
  def flatMap[A,B](pa: Par[A])(f: A => Par[B]): Par[B] = {
    es => {
      run(f(run(pa)(es).get))(es)
    }
  }
}

object ParTest extends App {
  import java.util.concurrent._
  implicit val es = Executors.newCachedThreadPool()
  val a = Par.unit({println("a "+Thread.currentThread.getName);4+7})
  val b = Par.async({println("b "+Thread.currentThread.getName);2+1})
  println(Par.run(a).get)
  println(Par.run(b).get)
  import Par._
  val r = Par.map2(async({println(Thread.currentThread.getName); 41+2}),
    async({println(Thread.currentThread.getName); 33+4}))
    {(a,b) => {println(Thread.currentThread.getName); a+b}}(es).get
  println(r)
  val r1 = fork { map2(async({println(Thread.currentThread.getName); 41+2}),
    async({println(Thread.currentThread.getName); 33+4}))
  {(a,b) => {println(Thread.currentThread.getName); a+b}}}(es).get
  println(r1)
  es.shutdown()
}