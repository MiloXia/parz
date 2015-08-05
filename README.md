# Parz
Free Monad for Par <br/>
I have not been idle :)

##Open mode

add type

```
  class IOReq[A] extends Request[A]
  case class Get(q: String) extends IOReq[String]
  case class Put(r: String) extends IOReq[Unit]
```

add function

```
  def get(q: String) = Get(q)
  def put(p: String) = Put(p)
  def add(b: String, c: String) = b + c
```

add for-comprehension <br/>
Monad is express dependent computation
Applicative is express parallel computation (independent computation)
```
val test = for {
    a <- get("get a")
    d <- Done(add _) <*> get("get b") <*> get("get c") // parallel computation
    _ <- put(a+d)
  } yield ()
```

add service

```
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

add effect & run

```
implicit val fetchEffect = FetchEffect(IOReqService)

test.run
```