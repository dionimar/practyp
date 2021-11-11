package example


//import cats.effect.unsafe.implicits.global
import cats.data.ContT
import cats.effect._
import scala.concurrent.duration._
import scala.io.StdIn._
import scala.math.pow

import com.googlecode.lanterna.terminal.Terminal
import com.googlecode.lanterna._
import com.googlecode.lanterna.input.KeyType

//import scala.utils.jline
import java.util.Scanner
import javax.swing.KeyStroke


object Main extends IOApp.Simple {
  val defaultTerminalFactory = new terminal.DefaultTerminalFactory()
  val term = defaultTerminalFactory.createTerminal()
  term.clearScreen()

  val textGraphics = term.newTextGraphics()

  //var keyStroke = term.readInput()

  val env = (term, textGraphics)

  def getCharAndContinue(
    inputString: String,
    typedString: String,
    env: (terminal.Terminal, graphics.TextGraphics),
    keyStroke: input.KeyStroke): IO[(String, (terminal.Terminal, graphics.TextGraphics))] =
  {
    val term = env._1
    val graphs = env._2
    keyStroke.getKeyType() match {
      case KeyType.Escape => IO {term.clearScreen(); (typedString, env)}
      case KeyType.Enter  => IO {term.clearScreen(); (typedString, env)}
      case KeyType.Delete | KeyType.Backspace => {
        for {
          _ <- IO(graphs.putString(5, 2, inputString, SGR.BOLD))
          newTypedString <- IO(typedString.dropRight(1))
          _ <- IO(graphs.putString(5, 4, newTypedString))
          _ <- IO(term.putCharacter(' '))
          _ <- IO(term.setCursorPosition(term.getCursorPosition().withRelativeColumn(-1)))
          _ <- IO(term.flush())
          newKeyStroke <- IO(term.readInput())
          typedResult <- getCharAndContinue(inputString, newTypedString, env, newKeyStroke)
        } yield ((typedResult._1, env))
      }
      case other => {
        for {
          _ <- IO(graphs.putString(5, 2, inputString, SGR.BOLD))
          newTypedString <- IO(typedString + keyStroke.getCharacter().toString())
          _ <- IO(graphs.putString(5, 4, newTypedString))
          _ <- IO(term.flush())
          newKeyStroke <- IO(term.readInput())
          typedResult <- getCharAndContinue(inputString, newTypedString, env, newKeyStroke)
        } yield ((typedResult._1, env))
      }
    }
  }

  val ks = term.readInput()
  def runTimer(typer: String) = for {
    start <- Clock[IO].monotonic
    res <- getCharAndContinue(typer, "", env, ks)
    typedString <- IO(res._1)
    env <- IO(res._2)
    ending <- Clock[IO].monotonic
  } yield(ending - start, typedString)

  val run = for {
    results <- runTimer("Type this as fast as you can")
    elapsedTime <- IO(results._1)
    typedString <- IO(results._2)
    _ <- IO(println(typedString.split(" ").size))
    // _ <- IO(println(typedResult))
    _ <- IO(println(elapsedTime.toSeconds + " WPM"))
    _ <- IO(println(typedString))
  } yield ()


}


// object Main extends IOApp.Simple {

//   // lazy val mainLoop: IO[Unit] = IO.println("hola") >> mainLoop
//   // mainLoop.timeout(5.seconds)

//   def elapsedSeconds(start: FiniteDuration, ending: FiniteDuration) =
//     ((ending - start)).toSeconds

//   def wordsPerMinute(inputString: String, duration: Double) =
//     inputString.split(" ").size * 60.0 / duration

//   def compare(input: String, output: String) =
//     input
//       .zip(output)
//       .map(x => x._1 == x._2)

//   def putStr(str: String) = IO(println(str))
//   def put_(str: String) = IO(print(str))
//   val charReader = System.console().reader()
//   //val charReader = new Scanner(System.in)
//   //val rdchar: IO[String] = IO(charReader.read.toChar.toString.mkString(""))


//   def rdline(inputString: String): String = {
//     Iterator
//       .continually(
//         {
//           val c = charReader.read;
//           //val c = charReader.next()
//           //print(c)
//           c.toString;
//         }
//       )
//       .takeWhile( s => s.toInt >= 32 && s.toInt <= 126 && s.nonEmpty)
//       .toList
//       .map(_.toInt.toChar.toString)
//       .mkString("")
//   }

//   def clearScreen = putStr("\033[2J")

//   def mainLoop() = for {
//     _ <- clearScreen
//     _ <- putStr("hola typea esto plz")
//     start <- Clock[IO].monotonic
//     typedText <- IO(rdline("hola typea esto plz"))
//     ending <- Clock[IO].monotonic
//     _ <- putStr(
//       wordsPerMinute(typedText, elapsedSeconds(start, ending)).toString + "WPM"
//     )
//   } yield ()

//   val run = mainLoop()

//   // val run = for {
//   //   results <- mainLoop
//   //   _ <- 
//   // } yield ()

  
// }
