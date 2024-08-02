package com.corem.part3fibers

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}
import scala.concurrent.duration._

object Fibers extends IOApp.Simple:

  val meaningOfLife = IO.pure(42)
  val favLang = IO.pure("Scala")

  import com.corem.utils._

  def simpleIOComposition() =
    for
      _ <- meaningOfLife.myDebug
      _ <- favLang.myDebug
    yield ()

  def createFiber: Fiber[IO, Throwable, String] =
    ??? // Almost impossible to create fibers manually

  // The fiber is not actually started but the fiber allocation is wrapped in another effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.myDebug.start

  def differentThreadIOs() =
    for
      _ <- aFiber
      _ <- favLang.myDebug
    yield ()

  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] =
    for
      fib <- io.start
      result <- fib.join
    yield result

  /*
    IO[ResultType of fib.join]
    fib.join = Outcome[IO, Throwable, A]
   */

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Succeeded(fa) => fa
    case Errored(e)    => IO(0)
    case Canceled()    => IO(0)
  }

  def throwOnAnotherThread() =
    for
      fib <- IO.raiseError[Int](new RuntimeException("No number for you")).start
      result <- fib.join
    yield result

  def testCancel() =
    val task =
      IO("Starting").myDebug >> IO.sleep(1.second) >> IO("Done").myDebug

    val taskWithCancellationHandler =
      task.onCancel(IO("I'm being cancelled").myDebug.void)

    for
      fib <- taskWithCancellationHandler.start
      _ <- IO.sleep(500.millis) >> IO("Cancelling").myDebug
      _ <- fib.cancel
      result <- fib.join
    yield result

  override def run: IO[Unit] =
    testCancel() // IO(Succeeded(IO(42)))
      .myDebug.void
