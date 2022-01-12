package practyp


// TypingTest represents the algebra
// F[_] stands for effects
trait TypingTest[F[_], Result, Target, TestProp] {
  def getResult(target: Target)(implicit presenter: Presenter[F]): F[TestProp]
  // compute a single test score
  def compScore(testProperties: TestProp): Summary
  // combine function to extract an aggregated score for current and previous one
  def combScores(scoreList: List[Option[Summary]]): Summary
}
// TypingTest properties should be defined in companion object
object TypingTest {}

trait Presenter[F[_]] {
  def flush(): F[Unit]
  def show(content: String): F[Unit]
  def showForInput(content: String): F[Unit]
  def getOption(): F[Either[Throwable, String]]
  def readInput(): F[String]
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
final case class TestProperties[Result, Target](
  input: Result, output: Target, elapsedTime: Double
)

sealed trait Score
final case class Summary(wpm: Double, accuracy: Double) extends Score {
  override def toString() = s"\n\t\t${wpm} WPM\n\t\t${accuracy} acc\n"
}
case object EmptyScore extends Score

