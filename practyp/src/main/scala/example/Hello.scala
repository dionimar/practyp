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




class termEnv(val term: terminal.Terminal, val graphs: graphics.TextGraphics)


object Main extends IOApp.Simple {
  val defaultTerminalFactory = new terminal.DefaultTerminalFactory()
  val term = defaultTerminalFactory.createTerminal()
  term.clearScreen()

  val textGraphics = term.newTextGraphics()

  val env = new termEnv(term, textGraphics)


  def deleteLastChar(env: termEnv): IO[Unit] = {
    for {
      _ <- IO(env.term.setCursorPosition(term.getCursorPosition().withRelativeColumn(-1)))
      _ <- IO(env.term.putCharacter(' '))
      _ <- IO(env.term.flush())
    } yield ()
  }

  def drawCurrentState(env: termEnv, inputString: String, typedString: String): IO[Unit] = {
    for {
      _ <- IO(env.graphs.putString(5, 2, inputString, SGR.BOLD))
      _ <- IO(env.graphs.putString(5, 4, typedString))
      _ <- IO(env.term.flush())
    } yield ()
  }

  def getCharAndContinue(
    inputString: String,
    typedString: String,
    env: termEnv,
    keyStroke: input.KeyStroke): IO[(String, termEnv)] =
  {
    keyStroke.getKeyType() match {
      case KeyType.Escape => IO {env.term.clearScreen(); (typedString, env)}
      case KeyType.Enter  => IO {env.term.clearScreen(); (typedString, env)}
      case KeyType.Delete | KeyType.Backspace => {
        val newTypedString = typedString.dropRight(1)
        for {
          _ <- deleteLastChar(env)
          _ <- drawCurrentState(env, inputString, newTypedString)
          newKeyStroke <- IO(env.term.readInput())
          typedResult <- getCharAndContinue(inputString, newTypedString, env, newKeyStroke)
        } yield ((typedResult._1, env))
      }
      case other => {
        val newTypedString = typedString + keyStroke.getCharacter().toString()
        for {
          _ <- drawCurrentState(env, inputString, newTypedString)
          newKeyStroke <- IO(env.term.readInput())
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
