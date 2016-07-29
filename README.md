# To Create a Scala SBT Project

```bash
mkdir -p src/main/scala
mkdir -p src/test/scala
git init
touch .gitignore
echo'.ensime_cache\nproject/\ntarget/\n' > .gitignore
```

# Key Types

### laziness

trait Stream[+A] {}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

## state

trait RNG { nextInt: (Int, RNG) }
RNG.Simple(seed: Long) extends RNG

case class State[RNG, +A](run: RNG => (A, RNG))

### parallelism

trait Future[+A] { def apply(a: A => Unit): Unit }
trait SafeFuture[+A] { def apply(a: Option[A] => Unit): Unit }
type Par[+A] = ExecutorService => Future[A]
type SafePar[+A] = ExecutorService => SafeFutre[A]
def Par.run[A](es: ExecutorService)(p: Par[A]): A
def Par.safeRun[A](es: ExecutorService)(p: SafePar[A]): Option[A]

### testing

case class Gen[+A](sample: State[RNG, A])
case class SGen[+A](forSize: Int => Gen[A])
case class Prop(run: (MaxSize, TestCases, RNG) => Result)
def Prop.run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit
def check(p: Boolean): Prop
def checkPar(p: Par.Par[Boolean]): Prop
def randomStream[A](g: Gen[A])(rng: RNG): Stream[A]
# To Get more comfortable with

```scala
Par.sequence
```
