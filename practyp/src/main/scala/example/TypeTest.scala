package practyp


// TypingTest represents the algebra
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






// ADT's: Algebraic Data Types 
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

