package effect

import effect.internal.IOExecutor

/**
  * 今回実装する IO は、
  * 1. pure を用いて IO にリフトできる。
  * 2. map, flatMap を実装して、コンビネータをつなげることができる。
  * 3. 処理を遅延させられる。
  * 4. 内部で例外を投げられる。その後は処理を継続しない。
  * という機能のみを実装した簡易版です。
  *
  * 主に教育用に cats.effect.IO を参考に実装されており、
  * IOExecutor と見比べながら、IO がステートマシンであることを
  * 体感してもらいやすいように、あえて多くを捨象しています。
  *
  * @tparam A
  */
sealed trait IO[+A] {

  import IO._

  def map[B](f: A => B): IO[B] = this match {
    case Map(original, g, index) if index != 128 =>
      Map(original, g.andThen(f), index + 1)
    case _ => Map(this, f, 0)
  }

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

  def unsafeRunSync(): A = {
    IOExecutor.step(this).asInstanceOf[IO[A]] match {
      case Pure(a)         => a
      case RaiseError(err) => throw err
      case _               => throw new AssertionError("unreachable")
    }
  }
}

object IO {

  def pure[A](a: A): IO[A] = Pure(a)

  def apply[A](a: => A): IO[A] = delay(a)

  def delay[A](a: => A): IO[A] = Delay(a _)

  def raiseError[A](err: Throwable): IO[A] = RaiseError(err)

  final case class Pure[+A](a: A) extends IO[A]
  final case class Delay[+A](thunk: () => A) extends IO[A]
  final case class RaiseError(err: Throwable) extends IO[Nothing]
  final case class FlatMap[E, +A](original: IO[E], f: E => IO[A]) extends IO[A]
  final case class Map[E, +A](original: IO[E], f: E => A, index: Int)
      extends IO[A]
      with (E => IO[A]) {
    override def apply(value: E): IO[A] = Pure(f(value))
  }
}
