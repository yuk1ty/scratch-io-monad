package effect.internal

import effect.IO

private[this] trait IOFrame[-A, +R] extends (A => R) { self =>
  def recover(e: Throwable): R
}

private[this] object IOFrame {

  final case class ErrorHandler[A](fe: Throwable => IO[A])
      extends IOFrame[A, IO[A]] {
    override def apply(a: A): IO[A] = IO.pure(a)
    override def recover(e: Throwable): IO[A] = fe(e)
  }
}
