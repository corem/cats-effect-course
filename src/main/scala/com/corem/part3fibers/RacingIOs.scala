package com.corem.part3fibers

import cats.effect.{IO, IOApp}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._


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
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)

    first.flatMap {
      case Left(mol) => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"Fav lang won: $lang")
    }
  }

  override def run: IO[Unit] =
    testRace().myDebug.void
}
