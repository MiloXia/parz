# Parz
Free Monad for Par <br/>
I have not been idle :)

##Open mode

1. Add type

```scala
  class IOReq[A] extends Request[A]
  case class Get(q: String) extends IOReq[String]
  case class Put(r: String) extends IOReq[Unit]
```

2. Add functions

```scala
  def get(q: String) = Get(q)
  def put(p: String) = Put(p)
  def add(b: String, c: String) = b + c
```

3. Add for-comprehension <br/>

```scala
val test = for {
    a <- get("get a")
    d <- Done(add _) <*> get("get b") <*> get("get c")
    _ <- put(a+d)
  } yield ()
```

Monad is express dependent computation <br/>
Applicative is express parallel computation (independent computation)<br/>

4. Add service

```scala
object IOReqService extends Service[IOReq] {
    override def fetch[A](req: IOReq[A]): A = req match {
      case Get(q) =>
        println(q)
        Random.nextString(10)
      case Put(s) =>
        println(s)
    }
  }

```

5. Add effect & run

```scala
implicit val fetchEffect = FetchEffect(IOReqService)

test.run
```