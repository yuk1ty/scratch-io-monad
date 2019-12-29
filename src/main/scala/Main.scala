import effect.IO

object Main extends App {

  val io = for {
    num <- IO.pure(123)
    numStr <- IO.pure(num.toString)
    withHello <- IO.pure(s"${numStr}, Hello!")
    _ <- IO(println(withHello))
  } yield ()

	// "123, Hello!"
  io.unsafeRunSync()
}
