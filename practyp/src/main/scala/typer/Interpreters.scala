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
    def tabulatedShow(str: String) = "\t" + str
  }

  

  implicit val typingTestImplicit: TypingTest[IO, String, String, TestProperties[String, String]] =
    new TypingTest[IO, String, String, TestProperties[String, String]] {
      def getResult(target: String)(implicit presenter: Presenter[IO]): IO[TestProperties[String, String]] = {
        for {
          _ <- presenter.show(shellPrettyShow.clearScreen + shellPrettyShow.tabulatedShow(target))
          timerStart <- Clock[IO].monotonic
          res <- presenter.readInput()
          timerEnd <- Clock[IO].monotonic
        } yield(TestProperties[String, String](target, res, (timerEnd - timerStart).toSeconds.toDouble))
      }

      def compScore(testProperties: TestProperties[String, String]): Score = {
        val wpm = testProperties.input.split(" ").size.toDouble * 60 / testProperties.elapsedTime
        val compStrings = testProperties.output
          .zipAll(testProperties.input, "", "")
          .map(x => x._1 == x._2)
        val acc = 100 * compStrings.count(_ == true).toDouble / compStrings.length.toDouble
        new Summary(wpm, acc)
      }
    }

  implicit val presenter: Presenter[IO] = new Presenter[IO] {
    def show[T](content: T): IO[Unit] = IO(println(content))
    def getOption(): IO[Either[Throwable, String]] = IO(Try(readChar().toString).toEither)
    def readInput(): IO[String] = for {
      _ <- IO(print(shellPrettyShow.tabulatedShow("")))
      output <- IO(readLine())
    } yield(output)
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

  implicit val implTargetSpace: Map[String, Int] = mapTargetInstance.internalMap
  implicit def getNextTargetString(tg: Map[String, Int], n: Int): String = mapTargetInstance.next(n)
}
