package practyp.TypeTest

import cats.effect._
import cats.Monad
import cats.implicits._



// TypingTest represents the algebra ////////////////////////////////////////////
// F[_] stands for effects
trait TypingTest[F[_], Result, Target, TestProp] {
  //type TP = TestProperties[Result, Target]
  def getResult(target: Target): F[TestProp]
  def compScore(testProperties: TestProp): Score
}
// TypingTest properties should be defined in companion object
object TypingTest {}

trait Presenter[F[_]] {
  def show[T](content: T): F[Unit]
}
object Presenter {
  def apply[F[_]]()(implicit ev: Presenter[F]): Presenter[F] = ev
}

trait Timer[T[_]] {
  def now[T[_]](): T[Double]
}
object Timer {
  def apply[T[_]]()(implicit ev: Timer[T]): Timer[T] = ev
}
/////////////////////////////////////////////////////////////////////////////// 





// ADT's: Algebraic Data Types ////////////////////////////////////////////////
final case class SummaryTest[Result, Target, Score](
  typedResult: Result,
  target: Target,
  score: Score
)

//sealed trait ExecTestProperties
final case class TestProperties[Result, Target](
  input: Result, output: Target, elapsedTime: Double
)
//case object InitTestProperties extends ExecTestProperties

sealed trait Score
final case class Summary(wpm: Double, accuracy: Double) extends Score
case object EmptyScore extends Score
/////////////////////////////////////////////////////////////////////////////// 



// Programs ///////////////////////////////////////////////////////////////////
// Use of algebra to define how the typing test works in terms of functions and data types
object Program {
  def runTest[T[_]: Monad, Result, Target, TestProp](testInput: Target)
    (implicit test: TypingTest[T, Result, Target, TestProp],
      presenter: Presenter[T],
    ): T[Unit] = {
    for {
      res <- test.getResult(testInput)
      score <- Monad[T].pure(test.compScore(res))
      _ <- presenter.show(score)
    } yield()
  }
}
/////////////////////////////////////////////////////////////////////////////// 



// Interpreter ////////////////////////////////////////////////////////////////
// Implementation specific code goes here
object Interpreter {

  import scala.io.StdIn.readLine
  import scala.concurrent.duration._

  type Res = String
  type Targ = String

  implicit val typ: TypingTest[IO, String, String, TestProperties[String, String]] =
    new TypingTest[IO, String, String, TestProperties[String, String]] {
      def getResult(target: String): IO[TestProperties[String, String]] = {
        for {
          _ <- IO(println(target))
          timerStart <- Clock[IO].monotonic
          res <- IO(readLine().toString())
          timerEnd <- Clock[IO].monotonic
        } yield(TestProperties[String, String](target, res, 0))
      }

      def compScore(testProperties: TestProperties[String, String]): Score =
        new Summary(testProperties.elapsedTime, 0)
    }
}
/////////////////////////////////////////////////////////////////////////////// 
