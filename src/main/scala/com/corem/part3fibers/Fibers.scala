package com.corem.part3fibers

import cats.effect.IO.{IOCont, Uncancelable}
import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}

import scala.concurrent.duration.*

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

  /*
    Exercices
    1. Write a function that runs an IO on another thread
      - Return the result in an IO
      - if Errored or Cancelled, return a failed IO
    2. Write a function that takes two IOs, runs them on different fibers and returns an
      IO with a tuple containing both results
      - If both IOs complete successfully, tuple their results
      - If the first IO returns an error, raise that error (ignoring the second IO)
      - If the first IO doesn't error but second IO returns an error, raise thqt error
      - If one (or both) cancelled raise a RuntimeException
    3. Write a function that adds a timeout to an IO:
      - IO runs on a fiber
      - If the timeout duration passes, then the fiber is cancelled
      - The method returns an IO[A] which contains
        - The original value if the computation is successful before the timeout signal
        - The exception if the computation is failed before the timeout signal
        - a RuntimeException if it times out
   */

  def processResultFromFiber[A](ioa: IO[A]): IO[A] =
    val ioResult = for
      fib <- ioa.myDebug.start
      result <- fib.join
    yield result

    ioResult.flatMap{
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation Canceled"))
    }

  def testEx1() =
    val aComputation = IO("Starting").myDebug >> IO.sleep(1.second) >> IO("Done!").myDebug >> IO(42)
    processResultFromFiber(aComputation).void

  def tupleIOs[A,B](ioa: IO[A], iob: IO[B]): IO[(A,B)] =
    val ioResult =
      for
        fiba <- ioa.start
        fibb <- iob.start
        resulta <- fiba.join
        resultb <- fibb.join
      yield (resulta, resultb)

    ioResult.flatMap {
      case (Succeeded(fa), Succeeded(fb)) =>
        for
          a <- fa
          b <- fb
        yield (a,b)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _ => IO.raiseError(RuntimeException("Some computation canceled"))
    }

  def testEx2() =
    val firstIO = IO.sleep(2.seconds) >> IO(1).myDebug
    val secondIO = IO.sleep(3.seconds) >> IO(2).myDebug
    tupleIOs(firstIO, secondIO).myDebug.void

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    val computation =
      for
        fib <- io.start
        _ <- IO.sleep(duration) >> fib.cancel
        result <- fib.join
      yield result

    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(RuntimeException("Computation Canceled"))
    }

  def testEx3() =
    val aComputation = IO("Starting").myDebug >> IO.sleep(1.second) >> IO("Done!").myDebug >> IO(42)
    timeout(aComputation, 0.5.seconds).myDebug.void

  override def run: IO[Unit] = testEx3()
