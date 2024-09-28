package com.corem.part4coordination

import cats.effect.{IO, IOApp, Ref}
import com.corem.utils._

import scala.concurrent.duration._

object Refs extends IOApp.Simple {

  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicMolV2: IO[Ref[IO, Int]] = IO.ref(42)

  val increasedMol: IO[Unit] = atomicMol.flatMap { ref =>
    ref.set(43)
  }

  val mol = atomicMol.flatMap { ref =>
    ref.get
  }

  val gsMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.getAndSet(43)
  }

  val fMol: IO[Unit] = atomicMol.flatMap { ref =>
    ref.update(value => value * 10)
  }

  val updatedMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.updateAndGet(value => value * 10)
  }

  val modifiedMol: IO[String] = atomicMol.flatMap { ref =>
    ref.modify(value => (value * 10, s"My current value is $value"))
  }

  import cats.syntax.parallel._

  val catsEffect = "I love Cats Effect"
  val refs = "This Ref thing is useless"
  val code = "We write a lot of code"
  def demoConcurrentWorkImpure(): IO[Unit] = {
    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount'").myDebug
        newCount <- IO(count + wordCount)
        _ <- IO(s"New total: $newCount").myDebug
        _ <- IO(count += wordCount)
      } yield ()
    }

    List(catsEffect, refs, code)
      .map(task)
      .parSequence
      .void
  }

  def demoConcurrentWorkPure(): IO[Unit] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length

      for {
        _ <- IO(s"Counting words for '$workload': $wordCount'").myDebug
        newCount <- total.updateAndGet(currentCount => currentCount + wordCount)
        _ <- IO(s"New total: $newCount").myDebug
      } yield ()
    }

    for {
      initialCount <- Ref[IO].of(0)
      _ <- List(catsEffect, refs, code)
        .map(string => task(string, initialCount))
        .parSequence
    } yield ()
  }

  /*
    Exercises
  */
  def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L
    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).myDebug
      _ <- IO(ticks += 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"Ticks: $ticks").myDebug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  def tickingClockPure(): IO[Unit] = {
    def tickingClock(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).myDebug
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      t <- ticks.get
      _ <- IO(s"Ticks: $t").myDebug
      _ <- printTicks(ticks)
    } yield ()

    for {
      tickRef <- Ref[IO].of(0)
      _ <- (tickingClock(tickRef), printTicks(tickRef)).parTupled
    } yield ()
  }

  def tickingClockWeird(): IO[Unit] = {
    val ticks = Ref[IO].of(0)

    def tickingClock: IO[Unit] = for {
      t <- ticks
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).myDebug
      _ <- t.update(_ + 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      t <- ticks
      _ <- IO.sleep(5.seconds)
      currentTicks <- t.get
      _ <- IO(s"Ticks: $currentTicks").myDebug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  override def run = tickingClockWeird()
}