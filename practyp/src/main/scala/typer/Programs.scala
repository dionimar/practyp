// Programs ///////////////////////////////////////////////////////////////////
// Use of algebra to define how the typing test works in terms of functions and data types

package practyp

import cats.Monad
import cats.implicits._




object Programs {

  def runTest[T[_]: Monad, Result, Target, TestProp](testInput: Target)
    (implicit test: TypingTest[T, Result, Target, TestProp],
      presenter: Presenter[T]
    ): T[TestProp] = {
    for {
      res <- test.getResult(testInput)
      //score <- Monad[T].pure(test.compScore(res))
      //score <- test.compScore(res)
      //_ <- presenter.show(score)
    } yield(res)
  }

  def testRunner[T[_]: Monad, Result, Target, TgSpace[Target], TestProp](n: Int)
    (implicit test: TypingTest[T, Result, Target, TestProp],
      presenter: Presenter[T],
      targetSpace: TgSpace[Target],
      getNextTarget: (TgSpace[Target], Int) => Target
    ): T[Summary] = {
    val nextTest = getNextTarget(targetSpace, n)
    for {
      testResult <- runTest[T, Result, Target, TestProp](nextTest)
      testScore <- Monad[T].pure(test.compScore(testResult))
      //_ <- presenter.show(testScore)
    } yield(testScore)
  }

  def askMenu[T[_]: Monad, Result, Target, TgSpace[Target], TestProp](lastScore: Option[Summary])
    (implicit test: TypingTest[T, Result, Target, TestProp],
      presenter: Presenter[T],
      targetSpace: TgSpace[Target],
      getNextTarget: (TgSpace[Target], Int) => Target
    ): T[Unit] = {
    for {
      testScore <- testRunner[T, Result, Target, TgSpace, TestProp](10)
      _ <- presenter.show(testScore)
      //_ <- presenter.show(lastScore.getOrElse(Summary(0, 0)))
      c <- Monad[T].pure(test.combScores(testScore, lastScore.getOrElse(testScore)))
      _ <- presenter.show(c)
      _ <- presenter.show("Type q to quit")
      choice <- presenter.getOption()
      _ <- choice match {
        case Right("q") => Monad[T].pure({})
        case other => askMenu[T, Result, Target, TgSpace, TestProp](Some(testScore))
      }
    } yield()
  }

}
