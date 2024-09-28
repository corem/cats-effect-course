package com.corem.part4coordination


import cats.effect.kernel.Outcome
import cats.effect.{Deferred, Fiber, IO, IOApp, Ref}
import com.corem.utils._

import scala.concurrent.duration._
import cats.syntax.traverse._

object Defers extends IOApp.Simple {



  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val aDeferredV2: IO[Deferred[IO, Int]] = IO.deferred[Int] // same

  val reader: IO[Int] = aDeferred.flatMap { signal =>
    signal.get
  }

  val writer = aDeferred.flatMap { signal =>
    signal.complete(42)
  }

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[Consumer] Waiting for result...").myDebug
      meaningOfLife <- signal.get
      _ <- IO(s"[Consumer] Got the result: $meaningOfLife").myDebug
    } yield ()

    def producer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[Producer] Crunching numbers...").myDebug
      _ <- IO.sleep(1.second)
      _ <- IO("[Producer] Complete: 42").myDebug
      meaningOfLife <- IO(42)
      _ <- signal.complete(meaningOfLife)
    } yield ()

    for {
      signal <- Deferred[IO, Int]
      fibConsumer <- consumer(signal).start
      fibProducer <- producer(signal).start
      _ <- fibProducer.join
      _ <- fibConsumer.join
    } yield ()
  }

  val fileParts = List("I ", "love S", "cala", " with Cat", "s Effect!<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts
        .map { part =>
          IO(s"[Downloader] Got '$part'").myDebug >> IO.sleep(1.second) >> contentRef.update(currentContent => currentContent + part)
        }
        .sequence
        .void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get
      _ <- if (file.endsWith("<EOF>")) IO("[Notifier] File download completed").myDebug
      else IO("[notifier] Downloading...").myDebug >> IO.sleep(500.millis) >> notifyFileComplete(contentRef)
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      fibDownloader <- downloadFile(contentRef).start
      notifier <- notifyFileComplete(contentRef).start
      _ <- fibDownloader.join
      _ <- notifier.join
    } yield ()
  }

  def fileNotifierWithDeferred(): IO[Unit] = {
    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO("[Notifier] Downloading...").myDebug
      _ <- signal.get
      _ <- IO("[Notifier] File download completed").myDebug
    } yield ()

    def downloadFilePart(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO(s"[downloader] got '$part'").myDebug
      _ <- IO.sleep(1.second)
      latestContent <- contentRef.updateAndGet(currentContent => currentContent + part)
      _ <- if (latestContent.contains("<EOF>")) signal.complete(latestContent) else IO.unit
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      signal <- Deferred[IO, String]
      notifierFib <- notifyFileComplete(signal).start
      fileTasksFib <- fileParts.map(part => downloadFilePart(part, contentRef, signal)).sequence.start
      _ <- notifierFib.join
      _ <- fileTasksFib.join
    } yield ()
  }

  /*
   Exercises:
     - Write a small alarm notification with two simultaneous IOs
       - One that increments a counter every second (a clock)
       - One that waits for the counter to become 10, then prints a message "time's up!"

     - Implement racePair with Deferred.
       - Use a Deferred which can hold an Either[outcome for ioa, outcome for iob]
       - Start two fibers, one for each IO
       - On completion (with any status), each IO needs to complete that Deferred
       - What do you do in case of cancellation ?
   */
  // 1
  def eggBoiler(): IO[Unit] = {
    def eggReadyNotification(signal: Deferred[IO, Unit]) = for {
      _ <- IO("Egg boiling on some other fiber, waiting...").myDebug
      _ <- signal.get
      _ <- IO("EGG READY!").myDebug
    } yield ()

    def tickingClock(counter: Ref[IO, Int], signal: Deferred[IO, Unit]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      count <- counter.updateAndGet(_ + 1)
      _ <- IO(count).myDebug
      _ <- if (count >= 10) signal.complete(()) else tickingClock(counter, signal)
    } yield ()

    for {
      counter <- Ref[IO].of(0)
      signal <- Deferred[IO, Unit]
      notificationFib <- eggReadyNotification(signal).start
      clock <- tickingClock(counter, signal).start
      _ <- notificationFib.join
      _ <- clock.join
    } yield ()
  }

  type RaceResult[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]),
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B])
  ]

  type EitherOutcome[A, B] = Either[Outcome[IO, Throwable, A], Outcome[IO, Throwable, B]]

  def ourRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = IO.uncancelable { poll =>
    for {
      signal <- Deferred[IO, EitherOutcome[A, B]]
      fiba <- ioa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
      fibb <- iob.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
      result <- poll(signal.get).onCancel {
        for {
          cancelFibA <- fiba.cancel.start
          cancelFibB <- fibb.cancel.start
          _ <- cancelFibA.join
          _ <- cancelFibB.join
        } yield ()
      }
    } yield result match {
      case Left(outcomeA) => Left((outcomeA, fibb))
      case Right(outcomeB) => Right((fiba, outcomeB))
    }
  }


  override def run = eggBoiler()
}