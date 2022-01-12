package practyp


// TypingTest represents the algebra
// F[_] stands for effects
trait Tester[F[_], Result, Target, TestProp] {
  def getResult(target: Target)(implicit presenter: Presenter[F]): F[TestProp]
  // compute a single test score
  def compScore(testProperties: TestProp): Summary
  // combine function to extract an aggregated score for current and previous one
  def combScores(scoreList: List[Option[Summary]]): Summary
}
// TypingTest properties should be defined in companion object
// object Tester {}

trait Presenter[F[_]] {
  def flush(): F[Unit]
  def show(content: String): F[Unit]
  def showSummary(summ: Summary): F[Unit]
  def showForInput(content: String): F[Unit]
  def getOption(): F[Either[Throwable, String]]
  def readInput(): F[String]
}

trait Timer[T[_]] {
  def now[T[_]](): T[Double]
}
object Timer {
  def apply[T[_]]()(implicit ev: Timer[T]): Timer[T] = ev
}



final case class TestProperties[Result, Target](
  input: Result, output: Target, elapsedTime: Double
)

final case class Summary(wpm: Double, accuracy: Double) {
  override def toString() = s"\n\t\t${wpm} WPM\n\t\t${accuracy} acc\n"
}
