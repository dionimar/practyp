package example


//import cats.data.ContT
import cats.effect._
import scala.concurrent.duration._
import scala.io.StdIn._

import scala.math.pow

// import scala.swing._
// import scala.swing.event._

import scala.tools.jline._



object Main extends IOApp.Simple {

  // lazy val mainLoop: IO[Unit] = IO.println("hola") >> mainLoop
  // mainLoop.timeout(5.seconds)

  def elapsedSeconds(start: FiniteDuration, ending: FiniteDuration) =
    ((ending - start)).toSeconds

  def wordsPerMinute(inputString: String, duration: Double) =
    inputString.split(" ").size * 60 / duration

  def compare(input: String, output: String) =
    input
      .zip(output)
      .map(x => x._1 == x._2)

  // val consoleRead = new console.ConsoleReader()

  def putStr(str: String) = IO(println(str))
  def put_(str: String) = IO(print(str))
  val charReader = System.console().reader()
  val rdchar = IO(charReader.read.toString)
  //val rdchar = IO(readChar())
  //val rdchar = IO(consoleRead.readCharacter())
  // val rdline = IO(readLine())

  val rdline: IO[String] = IO {
      Iterator
        .continually(
          {
            val c = charReader.read
            //print(c.toChar)
            c.toString
          }
        )
        .takeWhile( s => s.toInt >= 32 && s.toInt <= 126 && s.nonEmpty)
        .toList
        .map(_.toInt.toChar.toString)
        .mkString("")
  }
  

  def clearScreen = putStr("\033[2J")

  // def performIteration(testString: String) = for {
  //   _ <- clearScreen
  //   _ <- putStr(testString)
  //   start <- Clock[IO].monotonic
  //   x <- IO(rdline)
  //   ending <- Clock[IO].monotonic
  //   results <- IO(wordsPerMinute(x, elapsedSeconds(start, ending)).toString + "WPM") 
  // } yield results

  val mainLoop: IO[Unit] = for {
    // results <- performIteration("Type this if you can as fast as you can")
    // _ <- putStr(results)


    _ <- clearScreen
    _ <- putStr("hola typea esto plz")
    start <- Clock[IO].monotonic
    x <- rdline
    //x <- IO.iterateWhile()
    ending <- Clock[IO].monotonic
    //_ <- putStr(x)
    _ <- putStr(wordsPerMinute(x, elapsedSeconds(start, ending)).toString + "WPM")

    //wait <- rdline

    // results <- performIteration("En un lugar de la mancha de cuyo nombre no macuerdo")
    // _ <- putStr(results)
  } yield ()

  val run = mainLoop
}
