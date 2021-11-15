// Programs ///////////////////////////////////////////////////////////////////
// Use of algebra to define how the typing test works in terms of functions and data types

package practyp

//import TypeTest._

import cats.Monad
import cats.implicits._ 



object Programs {
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
