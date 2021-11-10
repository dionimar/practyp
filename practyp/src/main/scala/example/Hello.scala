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


object Main extends App {
  val defaultTerminalFactory = new terminal.DefaultTerminalFactory()
  val term = defaultTerminalFactory.createTerminal()
  term.clearScreen()

  val textGraphics = term.newTextGraphics()

  //var keyStroke = term.readInput()

  def getCharAndContinue(
    inputString: String,
    term: terminal.Terminal,
    keyStroke: input.KeyStroke
  ): Unit = {
    keyStroke.getKeyType() match {
      case KeyType.Escape => {}
      case other => {
        textGraphics.drawLine(5, 4, term.getTerminalSize().getColumns() - 1, 4, ' ')
        textGraphics.putString(5, 4, "Last Keystroke: ", SGR.BOLD)
        textGraphics.putString(5 + "Last Keystroke: ".length(), 4, keyStroke.toString())
        term.flush()
        val newKeyStroke = term.readInput()
        getCharAndContinue(inputString, term, newKeyStroke)
      }
    }
  }

  val ks = term.readInput()
  getCharAndContinue("asdf df asdf", term, ks)

  // Iterator.continually(
  //   {
  //     val keyStroke = term.readInput()
  //     textGraphics.drawLine(5, 4, term.getTerminalSize().getColumns() - 1, 4, ' ')
  //     textGraphics.putString(5, 4, "Last Keystroke: ", SGR.BOLD)
  //     textGraphics.putString(5 + "Last Keystroke: ".length(), 4, keyStroke.toString())
  //     term.flush()
  //     keyStroke
  //   }
  // )
  //   .takeWhile(_.getKeyType() != KeyType.Escape)
  //   .toList

  // while(keyStroke.getKeyType() != KeyType.Escape) {
  //   textGraphics.drawLine(5, 4, term.getTerminalSize().getColumns() - 1, 4, ' ');
  //   textGraphics.putString(5, 4, "Last Keystroke: ", SGR.BOLD);
  //   textGraphics.putString(5 + "Last Keystroke: ".length(), 4, keyStroke.toString());
  //   term.flush();
  //   keyStroke = term.readInput();
  // }

  // val in = Iterator.continually(

  // )


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
