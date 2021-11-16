package practyp

import cats.effect._
import cats.implicits._

import practyp.Interpreters._
import practyp.Programs._



// color examples
// "\u001B[38;5;76m"


object Main extends IOApp.Simple {
  def run() = {
    for {
      _ <- Programs.askMenu[IO, String, String, List, TestProperties[String, String]]()
    } yield ()
  }
}
