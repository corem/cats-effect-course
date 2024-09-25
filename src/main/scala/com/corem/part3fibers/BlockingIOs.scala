package com.corem.part3fibers

import cats.effect.{IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
object BlockingIOs extends IOApp.Simple {

  import com.corem.utils._

  val someSleeps = for {
    _ <- IO
      .sleep(1.second)
      .myDebug
    _ <- IO.sleep(1.second).myDebug
  } yield ()

  val aBlockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computed a blocking code")
    42
  }

  val iosOnManyThreads = for {
    _ <- IO("First").myDebug
    _ <-
      IO.cede
    _ <- IO(
      "Second"
    ).myDebug
    _ <- IO.cede
    _ <- IO("Third").myDebug
  } yield ()

  def testThousandEffectsSwitch() = {
    val ec: ExecutionContext =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000)
      .map(IO.pure)
      .reduce(_.myDebug >> IO.cede >> _.myDebug)
      .evalOn(ec)
  }

  override def run = testThousandEffectsSwitch().void
}
