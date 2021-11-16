// Programs ///////////////////////////////////////////////////////////////////
// Use of algebra to define how the typing test works in terms of functions and data types

package practyp

import cats.Monad
import cats.implicits._




object Programs {

  def runTest[T[_]: Monad, Result, Target, TestProp](testInput: Target)
    (implicit test: TypingTest[T, Result, Target, TestProp],
      presenter: Presenter[T]
    ): T[Unit] = {
    for {
      res <- test.getResult(testInput)
      score <- Monad[T].pure(test.compScore(res))
      _ <- presenter.show(score)
    } yield()
  }

  def testRunner[T[_]: Monad, Result, Target, TgSpace[Target], TestProp](n: Int)
    (implicit test: TypingTest[T, Result, Target, TestProp],
      presenter: Presenter[T],
      targetSpace: TgSpace[Target],
      getNextTarget: (TgSpace[Target], Int) => Target
    ): T[Unit] = {
    val nextTest = getNextTarget(targetSpace, n)
    for {
      _ <- runTest[T, Result, Target, TestProp](nextTest)
    } yield()
  }

  def askMenu[T[_]: Monad, Result, Target, TgSpace[Target], TestProp]()
    (implicit test: TypingTest[T, Result, Target, TestProp],
      presenter: Presenter[T],
      targetSpace: TgSpace[Target],
      getNextTarget: (TgSpace[Target], Int) => Target
    ): T[Unit] = {
    for {
      _ <- testRunner[T, Result, Target, TgSpace, TestProp](10)
      _ <- presenter.show("Type q to quit")
      choice <- presenter.getOption()
      _ <- choice match {
        case Right("q") => Monad[T].pure({})
        case other => askMenu[T, Result, Target, TgSpace, TestProp]()
      }
    } yield()
  }

}
