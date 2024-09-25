package com.corem.part3fibers

import cats.effect.kernel.Outcome.*
import cats.effect.{IO, IOApp}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.*

object RacingIOs extends IOApp.Simple {

  import com.corem.utils._

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"Starting computation: $value").myDebug >>
        IO.sleep(duration) >>
        IO(s"Computation for $value: done") >>
        IO(value)
    ).onCancel(IO(s"Computation Canceled for $value").myDebug.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
//    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    val first: IO[Either[Int, String]] = unrace(meaningOfLife, favLang)

    first.flatMap {
      case Left(mol)   => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"Fav lang won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)
    val raceResult = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outMol, fibLang)) =>
        fibLang.cancel >> IO("MOL won").myDebug >> IO(outMol).myDebug
      case Right((fibMol, outLang)) =>
        fibMol.cancel >> IO("Language won").myDebug >> IO(outLang).myDebug
    }
  }

  /* Exercises:
   1. Implement a timeout pattern with race
   2. Implement a method to return a LOSING effect from a race (hint: use racePair)
   3. Implement race in terms of racePair
   */
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val timeoutEffect = IO.sleep(duration)
    val result = IO.race(io, timeoutEffect)

    result.flatMap {
      case Left(v) => IO(v)
      case Right(_) =>
        IO.raiseError(new RuntimeException("Computation timed out"))
    }
  }

  val myTask = IO.sleep(4.seconds) >> IO(42).myDebug
  val testTimeOut = timeout(myTask, 1.second)
  val testTimeOut2 = myTask.timeout(1.second)

  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibB)) =>
        fibB.join.flatMap {
          case Succeeded(resultEffect) =>
            resultEffect.map(result => Right(result))
          case Errored(e) => IO.raiseError(e)
          case Canceled() =>
            IO.raiseError(new RuntimeException("Loser canceled."))
        }
      case Right((fibA, _)) =>
        fibA.join.flatMap {
          case Succeeded(resultEffect) =>
            resultEffect.map(result => Left(result))
          case Errored(e) => IO.raiseError(e)
          case Canceled() =>
            IO.raiseError(new RuntimeException("Loser canceled."))
        }
    }
  }

  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) =>
        outA match {
          case Succeeded(effectA) => fibB.cancel >> effectA.map(a => Left(a))
          case Errored(e)         => fibB.cancel >> IO.raiseError(e)
          case Canceled() =>
            fibB.join.flatMap {
              case Succeeded(effectB) => effectB.map(b => Right(b))
              case Errored(e)         => IO.raiseError(e)
              case Canceled() =>
                IO.raiseError(
                  new RuntimeException("Both computations cancelled.")
                )
            }
        }
      case Right((fibA, outB)) =>
        outB match {
          case Succeeded(effectB) => fibA.cancel >> effectB.map(b => Right(b))
          case Errored(e)         => fibA.cancel >> IO.raiseError(e)
          case Canceled() =>
            fibA.join.flatMap {
              case Succeeded(effectA) => effectA.map(a => Left(a))
              case Errored(e)         => IO.raiseError(e)
              case Canceled() =>
                IO.raiseError(
                  new RuntimeException("Both computations cancelled.")
                )
            }
        }
    }
  }

  override def run: IO[Unit] =
//    testRacePair().void
//    timeout(IO.sleep(4.seconds) >> IO(42).myDebug, 3.seconds).void
//    testTimeOut.void
    testRace().myDebug.void
}
