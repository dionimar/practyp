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

  class termEnv(val term: terminal.Terminal, val graphs: graphics.TextGraphics)

  val env = new termEnv(term, textGraphics)

  def drawCurrentState(env: termEnv, inputString: String, typedString: String): IO[Unit] = {
    val term = env.term
    val graphs = env.graphs
    for {
      _ <- IO(graphs.putString(5, 2, inputString, SGR.BOLD))
      _ <- IO(graphs.putString(5, 4, typedString))
      _ <- IO(term.flush())
    } yield ()
  }

  def getCharAndContinue(
    inputString: String,
    typedString: String,
    env: termEnv,
    keyStroke: input.KeyStroke): IO[(String, termEnv)] =
  {
    val term = env.term
    val graphs = env.graphs
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
          newTypedString <- IO(typedString + keyStroke.getCharacter().toString())
          _ <- drawCurrentState(env, inputString, newTypedString)
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


