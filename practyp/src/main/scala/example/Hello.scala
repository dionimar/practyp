package example

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

  def startTyping(env: termEnv, inputString: String): IO[(String, Double)] = {
    for {
      _ <- drawCurrentState(env, inputString, "")
      kp <- IO(env.term.readInput())
      startTimer <- Clock[IO].monotonic
      typedResult <- getCharAndContinue(inputString, "", env, kp)
      endTimer <- Clock[IO].monotonic
    } yield ((typedResult._1, (endTimer - startTimer).toSeconds))
  }

  val run = for {
    results <- startTyping(env, "Type this as fast as you can")
    elapsedTime <- IO(results._2)
    typedString <- IO(results._1)
    _ <- drawCurrentState(env, typedString, "Elapsed time: " + elapsedTime + " s.")
  } yield ()
}
