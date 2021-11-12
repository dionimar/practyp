package example

import cats.data.ContT
import cats.effect._
import scala.concurrent.duration._
import scala.io.StdIn.{readLine => scalaReadLine, readChar => scalaReadChar}
//import scala.math.{pow, abs}
import scala.util.{Random, Try}

import scala.swing._

import com.googlecode.lanterna.terminal.Terminal
import com.googlecode.lanterna._
import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.input.KeyType




object randElems {
  def apply[T](nElems: Int, list: Seq[T]) = {
    lazy val listSize = list.length
    Iterator.continually(list(Random.nextInt(listSize))).take(nElems).toList
  }
}


object stringJoiner {
  def test(a: String, b: String) = "\u001B[38;5;76m" + a
  def join(a: String, b: String) =
    a.split(" ").zipAll(b.split(" "), "", "")
      .map(x => x._1 == x._2 match {
        case true => "\u001B[38;5;76m" + x._1
        case other => x._2 == "" match {
          case true => "\u001B[38;5;246m" + x._1
          case _ => "\u001B[38;5;161m" + x._1
        }
      })
      .mkString(" ") + "\u001B[0m"
}



trait faceEnvironment {
  def flush()
  def read()
  def clear()
  def readLine(): String
  def readChar(): Either[Throwable, String]
  def print(col: Int, row: Int, str: String)
  def delChar()
}

case object rawTerm extends faceEnvironment {
  def flush() = {}
  def read() = {}
  def clear() = {println("\u001b[2J")}
  def readLine() = scalaReadLine()
  def readChar() = Try(scalaReadChar().toString).toEither
  def print(col: Int, row: Int, str: String) =
    println(
        Iterator.continually({"\t"}).take(col).mkString("") + str
    )
  def delChar() = {}
}



class termEnv(val term: terminal.Terminal, val graphs: graphics.TextGraphics) extends faceEnvironment {
  def flush() = term.flush()
  def read() = term.readInput()
  def clear() = term.clearScreen()
  def readLine() = scalaReadLine()
  def readChar() = Try(scalaReadChar().toString).toEither
  def print(col: Int, row: Int, str: String) = graphs.putString(col, row, str)
  def delChar() = {
    term.setCursorPosition(term.getCursorPosition().withRelativeColumn(-1))
    term.putCharacter(' ')
  }
}

object termEnv {
  def apply() = {
    val defaultTerminalFactory = new terminal.DefaultTerminalFactory()
    val term = defaultTerminalFactory.createTerminal()
    val textGraphics = term.newTextGraphics()
    new termEnv(term, textGraphics)
  }
}


object Main extends IOApp.Simple {

  def deleteLastChar(env: faceEnvironment): IO[Unit] = {
    for {
      _ <- IO(env.delChar())
      _ <- IO(env.flush())
    } yield ()
  }

  def drawCurrentState(env: faceEnvironment, inputString: String, typedString: String): IO[Unit] = {
    val formatted = stringJoiner.join(inputString, typedString)
    for {
      _ <- IO(env.print(2, 2, inputString))
      _ <- IO(env.print(2, 4, typedString))
      _ <- IO(env.flush())
    } yield ()
  }

  def drawStatistics(env: faceEnvironment, inputString: String, targetString: String, time: Double): IO[Unit] = {
    val wpm = inputString.split(" ").size.toDouble * 60 / time
    val compStrings = targetString
      .zipAll(inputString, "", "")
      .map(
        x => x._1 == x._2 match {
          case true => 1
          case false => 0
        }
      )
    val acc = compStrings.sum.toDouble / compStrings.length.toDouble

    for {
      _ <- IO(env.print(2, 2, stringJoiner.join(inputString, targetString)))
      _ <- IO(env.print(2, 4, s"Speed ${wpm} WPM"))
      _ <- IO(env.print(2, 2, s"Accuracy ${acc} %"))
    } yield()
  }

  // def getCharAndContinue(
  //   inputString: String,
  //   typedString: String,
  //   env: termEnv,
  //   keyStroke: input.KeyStroke): IO[(String, termEnv)] =
  // {
  //   keyStroke.getKeyType() match {
  //     case KeyType.Escape => IO {env.clear(); (typedString, env)}
  //     case KeyType.Enter  => IO {env.clear(); (typedString, env)}
  //     case KeyType.Delete | KeyType.Backspace => {
  //       val newTypedString = typedString.dropRight(1)
  //       for {
  //         _ <- deleteLastChar(env)
  //         _ <- drawCurrentState(env, inputString, newTypedString)
  //         newKeyStroke <- IO(env.read())
  //         typedResult <- getCharAndContinue(inputString, newTypedString, env, newKeyStroke)
  //       } yield ((typedResult._1, env))
  //     }
  //     case other => {
  //       val newTypedString = typedString + keyStroke.getCharacter().toString()
  //       for {
  //         _ <- drawCurrentState(env, inputString, newTypedString)
  //         newKeyStroke <- IO(env.read())
  //         typedResult <- getCharAndContinue(inputString, newTypedString, env, newKeyStroke)
  //       } yield ((typedResult._1, env))
  //     }
  //   }
  // }

  def readLine(env: faceEnvironment): IO[String] = {
    for {
      typedString <- IO(env.readLine())
    } yield (typedString)
  }

  def startTyping(env: faceEnvironment, inputString: String): IO[(String, Double)] = {
    for {
      _ <- drawCurrentState(env, inputString, "")
      //kp <- IO(env.read())
      startTimer <- Clock[IO].monotonic
      //typedResult <- getCharAndContinue(inputString, "", env, kp) // If enable, take firs comp
      typedResult <- readLine(env)
      endTimer <- Clock[IO].monotonic
    } yield ((typedResult, (endTimer - startTimer).toSeconds))
  }

  def mainLoop(env: faceEnvironment): IO[Unit] = {
    val dictionary = SpanishWords.words
    val sampleWords = randElems(10, dictionary)
      .mkString(" ")
      .replaceAll("á", "a")
      .replaceAll("é", "e")
      .replaceAll("í", "i")
      .replaceAll("ó", "o")
      .replaceAll("ú", "u")
    for {
      _ <- IO(env.clear())
      results <- startTyping(env, sampleWords)
      elapsedTime <- IO(results._2)
      typedString <- IO(results._1)
      _ <- drawStatistics(env, sampleWords, typedString, elapsedTime)
    } yield ()
  }

  def askForChoice(env: faceEnvironment): IO[Unit] = for {
    choice <- IO(env.readChar())
    _ <- choice match {
      case Left(_) => askForChoice(env)
      case Right(ch) => ch match {
        case "q" => IO{}
        case "r" => run()
        case other => askForChoice(env)
      }
    }
  } yield()

  def run() = {
    val env = rawTerm
    for {
      _ <- mainLoop(env)
      _ <- IO(env.print(2, 8, "Type r to repeat, q to quit."))
      _ <- askForChoice(env)
    } yield ()
  }
}
