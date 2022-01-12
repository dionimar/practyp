// Interpreter ////////////////////////////////////////////////////////////////
// Implementation specific code goes here

package practyp
import cats.effect._
import cats.Monad
import cats.implicits._
import cats.Show



object Interpreters {

  import scala.io.StdIn.{readLine, readChar}
  import scala.concurrent.duration._
  import scala.util.{Random, Try}

  object shellPrettyShow {
    val clearScreen = "\u001b[2J"
    val tabularChar = "\t"
  }

  object TesterImpl extends Tester[IO, String, String, TestProperties[String, String]] {
    def getResult(target: String)(implicit presenter: Presenter[IO]): IO[TestProperties[String, String]] = {
        for {
          _ <- presenter.showForInput(target)
          timerStart <- Clock[IO].monotonic
          res <- presenter.readInput()
          timerEnd <- Clock[IO].monotonic
        } yield(TestProperties[String, String](target, res, (timerEnd - timerStart).toSeconds.toDouble))
      }

      def compScore(testProperties: TestProperties[String, String]): Summary = {
        val wpm = testProperties.input.split(" ").size.toDouble * 60 / testProperties.elapsedTime
        val compStrings = testProperties.output
          .zipAll(testProperties.input, "", "")
          .map(x => x._1 == x._2)
        val acc = 100 * compStrings.count(_ == true).toDouble / compStrings.length.toDouble
        new Summary(wpm, acc)
      }

      def combScores(scoreList: List[Option[Summary]]): Summary = {
        scoreList match {
          case None :: Nil => Summary(0.0, 0.0)
          case _ => {
            val sl = scoreList.flatten
            val size = sl.length
            val wpms = sl.map(x => x.wpm).foldRight(0.0)(_ + _)
            val accs = sl.map(x => x.accuracy).foldRight(0.0)(_ + _)
            new Summary(wpms/size, accs/size)
          }
        }
      }
  }

  object PresenterImpl extends Presenter[IO] {
    def flush(): IO[Unit] = IO(println(shellPrettyShow.clearScreen))
    def show(content: String): IO[Unit] = IO(println(content))
    def showForInput(content: String): IO[Unit] =
      IO(println(shellPrettyShow.tabularChar + content))
    def getOption(): IO[Either[Throwable, String]] = IO(Try(readChar().toString).toEither)
    def readInput(): IO[String] = for {
      _ <- IO(print(shellPrettyShow.tabularChar))
      output <- IO(readLine())
    } yield(output)
    def showSummary(summ: Summary): IO[Unit] = {
      for {
        _ <- IO(print(shellPrettyShow.tabularChar))
        _ <- IO(println("############################################################################"))
        _ <- IO(print(shellPrettyShow.tabularChar))
        _ <- IO(println(s"\t\t${summ.wpm} WPM\t|\t${summ.accuracy} acc"))
        _ <- IO(print(shellPrettyShow.tabularChar))
        _ <- IO(println("############################################################################\n"))
      } yield()
    }
  }

  class mapTargetSpace() {
    var internalMap: Map[String, Int] = SpanishWords.words.map(x => (x -> 1)).toMap
    def next(n: Int): String = {
      lazy val targetList = this.internalMap.toSeq
      lazy val listSize = targetList.length
      lazy val targets = targetList.toList.sortBy(_._2).take(n)
      this.internalMap = (this.internalMap.toSeq ++ targets.toSeq).groupBy(_._1).mapValues(x => x.map(_._2).sum).toMap
      targets.map(_._1).mkString(" ")
    }
  }

  val mapTargetInstance = new mapTargetSpace()


  implicit val typingTestImplicit: Tester[IO, String, String, TestProperties[String, String]] = TesterImpl
  implicit val presenter: Presenter[IO] = PresenterImpl
  implicit val implTargetSpace: Map[String, Int] = mapTargetInstance.internalMap
  implicit def getNextTargetString(tg: Map[String, Int], n: Int): String = mapTargetInstance.next(n)
}
