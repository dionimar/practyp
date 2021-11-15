// Interpreter ////////////////////////////////////////////////////////////////
// Implementation specific code goes here

package practyp
import cats.effect._
import cats.Monad
import cats.implicits._ 


object Interpreters {

  import scala.io.StdIn.readLine
  import scala.concurrent.duration._

  implicit val typ: TypingTest[IO, String, String, TestProperties[String, String]] =
    new TypingTest[IO, String, String, TestProperties[String, String]] {
      def getResult(target: String): IO[TestProperties[String, String]] = {
        for {
          _ <- IO(println(target))
          timerStart <- Clock[IO].monotonic
          res <- IO(readLine())
          timerEnd <- Clock[IO].monotonic
        } yield(TestProperties[String, String](target, res, (timerEnd - timerStart).toSeconds.toDouble))
      }

      def compScore(testProperties: TestProperties[String, String]): Score = {
        val wpm = testProperties.input.split(" ").size.toDouble * 60 / testProperties.elapsedTime
        val compStrings = testProperties.output
          .zipAll(testProperties.input, "", "")
          .map(
            x => x._1 == x._2 match {
              case true => 1
              case false => 0
            }
          )
        val acc = 100 * compStrings.sum.toDouble / compStrings.length.toDouble
        new Summary(wpm, acc)
      }
    }

  implicit val presenter: Presenter[IO] = new Presenter[IO] {
    def show[String](str: String) = {
      IO(println(str))
    }
  }
}
