package practyp

import cats.data.ContT
import cats.effect._
import cats.implicits._

import scala.concurrent.duration._
import scala.io.StdIn.{readLine => scalaReadLine, readChar => scalaReadChar}
//import scala.math.{pow, abs}
import scala.util.{Random, Try}

import scala.swing._

import com.googlecode.lanterna.terminal.Terminal
import com.googlecode.lanterna._
import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.input.KeyType


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
