package practyp.TypeTest

import cats.effect.IO




// F[_] stands for effects
trait TypingTest[F[_], Result, Target, ExecTestProperties] {
  def getResult(target: Target): F[ExecTestProperties]
  def compScore(testProperties: TestProperties[Result, Target]): Score
}

object TypingTest {}

final case class SummaryTest[Result, Target, Score](
  typedResult: Result,
  target: Target,
  score: Score
)


sealed trait ExecTestProperties
final case class TestProperties[Result, Target](
  input: Result, output: Target, elapsedTime: Double
) extends ExecTestProperties
case object InitTestProperties extends ExecTestProperties


sealed trait Score
final case class Summary(wpm: Double, accuracy: Double) extends Score
case object EmptyScore extends Score








// Interpreter
object Test {

  import scala.io.StdIn.readLine

  type Result = String
  type Target = String
  implicit val typ = new TypingTest[IO, Result, Target, ExecTestProperties] {
    def getResult(target: Target): IO[ExecTestProperties] = {
      for {
        _ <- IO(println(target))
        res <- IO(readLine().toString())
      } yield(TestProperties(target, res, 0))
    }

    def compScore(testProperties: TestProperties[Result, Target]): Score = new Summary(testProperties.elapsedTime, 0)
  }
  //def apply(target: Target) = TypingTest.
}
