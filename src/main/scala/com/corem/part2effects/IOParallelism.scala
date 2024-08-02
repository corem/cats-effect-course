package com.corem.part2effects

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple:
  val aliceIO = IO(s"[${Thread.currentThread().getName}] Alice")
  val bobIO = IO(s"[${Thread.currentThread().getName}] Bob")

  val composedIO =
    for
      alice <- aliceIO
      bob <- bobIO
    yield s"$alice and $bob love Rock the JVM"

  import com.corem.utils._
  import cats.syntax.apply._

  val meaningOfLife: IO[Int] = IO.delay(42)
  val favLang = IO.delay("String")

  val goalOfLife =
    (meaningOfLife.myDebug, favLang.myDebug).mapN((num, string) =>
      s"My goal in life is $num and $string"
    )

  // Convert a senquential IO to parallel IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.myDebug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.myDebug)

  import cats.effect.implicits._
  val goalInLifeParallel: IO.Par[String] =
    (parIO1, parIO2).mapN((num, string) =>
      s"My goal in life is $num and $string"
    )

  // Turn it back to Sequential
  val goalInLifeV2: IO[String] =
    Parallel[IO].sequential(goalInLifeParallel)

  // This pattern shorthand:
  import cats.syntax.parallel._
  val goalInLifeV3: IO[String] =
    (meaningOfLife.myDebug, favLang.myDebug).parMapN((num, string) =>
      s"My goal in life is $num and $string"
    )

  // Regarding failure:
  val aFailure: IO[String] =
    IO.raiseError(new RuntimeException("I can't do this!"))

  // Compose success + failure
  val parallelWithFailure =
    (meaningOfLife.myDebug, aFailure.myDebug).parMapN(_ + _)

  // Compose failure + failure
  val anotherFailure: IO[String] =
    IO.raiseError(new RuntimeException("Second failure"))
  val twoFailures: IO[String] =
    (aFailure.myDebug, anotherFailure.myDebug).parMapN(_ + _)
  val twoFailuresDelayed: IO[String] =
    (IO(Thread.sleep(1000)) >> aFailure.myDebug, anotherFailure.myDebug)
      .parMapN(_ + _)

  override def run: IO[Unit] =
    twoFailuresDelayed.myDebug.void
