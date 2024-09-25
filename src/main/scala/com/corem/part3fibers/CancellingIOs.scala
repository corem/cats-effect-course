package com.corem.part3fibers

import cats.effect.{IO, IOApp}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.*

object CancellingIOs extends IOApp.Simple {

  import com.corem.utils._

  /*
    Cancelling IOs
    - fib.cancel
    - IO.race & other APIs
    - manual cancellation
   */
  val chainOfIOs: IO[Int] =
    IO("waiting").myDebug >> IO.canceled >> IO(42).myDebug

  // Uncancelable
  // Example: online store, payment processor
  // Payment process must not be canceled
  val specialPaymentSystem = (
    IO("Payment running, don't cancel me").myDebug >>
      IO.sleep(1.second) >>
      IO("Payment completed.").myDebug
  ).onCancel(IO("Cancel !").myDebug.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem)
  val atomicPayment_v2 = specialPaymentSystem.uncancelable

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO(
      "Attempting cancellation..."
    ).myDebug >> fib.cancel
    _ <- fib.join
  } yield ()

  /*
    Example: authentication service.
    Two parts:
    - Input password can be cancelled, because otherwise we might block indefinitely on user input
    - Verify password, cannot be cancelled once it's started
   */
  val inputPassword = IO("Input password:").myDebug >> IO(
    "Typing password"
  ).myDebug >> IO.sleep(2.seconds) >> IO("MySecurePassword")
  val verifyPassword = (pw: String) =>
    IO("Verifying...").myDebug >> IO.sleep(2.seconds) >> IO(
      pw == "MySecurePassword"
    )

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(
        IO("Authentication timed out. Try again later.").myDebug.void
      )
      verified <- verifyPassword(pw)
      _ <-
        if (verified)
          IO("Authentication successful.").myDebug
        else IO("Authentication failed.").myDebug
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.seconds) >> IO(
      "Authentication timeout. Attempting cancel..."
    ).myDebug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  /*
    Uncancelable calls are masks which suppress cancellation.
    Poll calls are "gaps opened" in the uncancelable region.
   */

  /* Exercises: what do you think the following effects will do?
    - 1. Anticipate
    - 2. Run to see if you're correct
    - 3. Prove your theory
   */

  val cancelBeforeMol = IO.canceled >> IO(42).myDebug
  val uncancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).myDebug)

  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(1.seconds) >> IO(
      "Authentication timeout, attempting cancel..."
    ).myDebug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(
        IO("Cancelable").myDebug >> IO.sleep(1.second) >> IO(
          "Cancelable end"
        ).myDebug
      ) >>
        IO("Uncancelable").myDebug >> IO.sleep(1.second) >> IO(
          "Uncancelable end"
        ).myDebug >>
        poll(
          IO("Second cancelable").myDebug >> IO.sleep(1.second) >> IO(
            "Second cancelable end"
          ).myDebug
        )
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(1500.millis) >> IO("Cancelling").myDebug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run = threeStepProgram()
}
