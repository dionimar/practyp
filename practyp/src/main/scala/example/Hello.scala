package example


//import cats.effect.unsafe.implicits.global
//import cats.data.ContT
import cats.effect._
import scala.concurrent.duration._
import scala.io.StdIn._
import scala.math.pow



object Main extends IOApp.Simple {

  // lazy val mainLoop: IO[Unit] = IO.println("hola") >> mainLoop
  // mainLoop.timeout(5.seconds)

  def elapsedSeconds(start: FiniteDuration, ending: FiniteDuration) =
    ((ending - start)).toSeconds

  def wordsPerMinute(inputString: String, duration: Double) =
    inputString.split(" ").size * 60.0 / duration

  def compare(input: String, output: String) =
    input
      .zip(output)
      .map(x => x._1 == x._2)

  def putStr(str: String) = IO(println(str))
  def put_(str: String) = IO(print(str))
  val charReader = System.console().reader()
  val rdchar: IO[String] = IO(charReader.read.toChar.toString.mkString(""))


  def rdline(inputString: String): String = {
    Iterator
      .continually(
        {
          val c = charReader.read;
          c.toString;
        }
      )
      .takeWhile( s => s.toInt >= 32 && s.toInt <= 126 && s.nonEmpty)
      .toList
      .map(_.toInt.toChar.toString)
      .mkString("")
  }

  def clearScreen = putStr("\033[2J")

  val mainLoop = for {
    _ <- clearScreen
    _ <- putStr("hola typea esto plz")
    start <- Clock[IO].monotonic
    typedText <- IO(rdline("hola typea esto plz"))
    ending <- Clock[IO].monotonic
  } yield (typedText, start, ending)

  //val run = mainLoop

  val run = for {
    results <- mainLoop
    _ <- IO(println(wordsPerMinute(results._1, elapsedSeconds(results._2, results._3)).toString + "WPM"))
  } yield ()

  
}
