package example


//import cats.data.ContT
import cats.effect._
import scala.concurrent.duration._
import scala.io.StdIn._







object Main extends IOApp.Simple {

  // lazy val mainLoop: IO[Unit] = IO.println("hola") >> mainLoop
  // mainLoop.timeout(5.seconds)

  def compare(input: String, output: String) =
    input
      .zip(output)
      .map(x => x._1 == x._2)

  def putStr(str: String) = IO(println(str))
  def put_(str: String) = IO(print(str))
  val rdchar = IO(readChar())
  val rdline = IO(readLine())

  val clock = Clock

  val mainLoop: IO[Unit] = for {
    _ <- putStr("Type this.")
    //start <- clock.monotonic(MILLISECONDS)
    x <- rdline
    _ <- putStr(compare(x, "Type this.").mkString(" "))
    //_ <- putStr(clock.monotonic(MILLISECONDS) - start)
  } yield ()

  val run = mainLoop
}
