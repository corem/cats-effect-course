package com.corem.part5polymorphic

import cats.{Applicative, Monad}
import cats.effect.{IO, IOApp, MonadCancel, Poll}
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}

object PolymorphicCancellation extends IOApp.Simple {

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def canceled: F[Unit]
    def uncancelable[A](poll: Poll[F] => F[A]): F[A]
  }

  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]

  val molIO: IO[Int] = monadCancelIO.pure(42)
  val ambitiousMolIO: IO[Int] = monadCancelIO.map(molIO)(_ * 10)

  val mustCompute = monadCancelIO.uncancelable { _ =>
    for {
      _ <- monadCancelIO.pure("Once started, I can't go back...")
      res <- monadCancelIO.pure(56)
    } yield res
  }

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def mustComputeGeneral[F[_], E](using mc: MonadCancel[F, E]): F[Int] = mc.uncancelable { _ =>
    for {
      _ <- mc.pure("Once started, I can't go back...")
      res <- mc.pure(56)
    } yield res
  }

  val mustCompute_v2 = mustComputeGeneral[IO, Throwable]

  val mustComputeWithListener = mustCompute.onCancel(IO("I'm being cancelled!").void)
  val mustComputeWithListener_v2 = monadCancelIO.onCancel(mustCompute, IO("I'm being cancelled!").void)
  import cats.effect.syntax.monadCancel._

  val aComputationWithFinalizers = monadCancelIO.guaranteeCase(IO(42)) {
    case Succeeded(fa) => fa.flatMap(a => IO(s"Successful: $a").void)
    case Errored(e) => IO(s"Failed: $e").void
    case Canceled() => IO("Canceled").void
  }

  val aComputationWithUsage = monadCancelIO.bracket(IO(42)) { value =>
    IO(s"Using the meaning of life: $value")
  } { value =>
    IO("Releasing the meaning of life...").void
  }

  /*
    Exercise: Generalize a piece of code (the auth-flow example from the Cancellation lesson)
   */
  import com.corem.utils.general._
  import scala.concurrent.duration._

  def unsafeSleep[F[_], E](duration: FiniteDuration)(using mc: MonadCancel[F, E]): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis))

  def inputPassword[F[_], E](using mc: MonadCancel[F,E]): F[String] = for {
    _ <- mc.pure("Input password:").myDebug
    _ <- mc.pure("(Typing password)").myDebug
    _ <- unsafeSleep[F, E](5.seconds)
    pw <- mc.pure("MySecurePassword1!")
  } yield pw

  def verifyPassword[F[_], E](pw: String)(using mc: MonadCancel[F, E]): F[Boolean] = for {
    _ <- mc.pure("Verifying...").myDebug
    _ <- unsafeSleep[F,E](2.seconds)
    check <- mc.pure(pw == "MySecurePassword1!")
  } yield check

  def authFlow[F[_], E](using mc: MonadCancel[F,E]): F[Unit] = mc.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(mc.pure("Authentication timed out. Try again later.").myDebug.void)
      verified <- verifyPassword(pw)
      _ <- if (verified) mc.pure("Authentication successful.").myDebug
      else mc.pure("Authentication failed.").myDebug
    } yield ()
  }

  val authProgram: IO[Unit] = for {
    authFib <- authFlow[IO, Throwable].start
    _ <- IO.sleep(3.seconds) >> IO("Authentication timeout, attempting cancel...").myDebug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  override def run = authProgram
}