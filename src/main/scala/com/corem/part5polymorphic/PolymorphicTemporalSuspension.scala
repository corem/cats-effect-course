package com.corem.part5polymorphic

import cats.effect.{IO, IOApp, Temporal}
import cats.effect.kernel.Concurrent

import scala.concurrent.duration.FiniteDuration
import com.corem.utils.general._
import scala.concurrent.duration._

object PolymorphicTemporalSuspension extends IOApp.Simple {

  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit]
  }

  val temporalIO = Temporal[IO]
  val chainOfEffects = IO("Loading...").myDebug *> IO.sleep(1.second) *> IO("Game ready!").myDebug
  val chainOfEffects_v2 = temporalIO.pure("Loading...").myDebug *> temporalIO.sleep(1.second) *> temporalIO.pure("Game ready!").myDebug

  /*
    Exercise: generalize the following piece
   */
  import cats.syntax.flatMap._
  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(using temporal: Temporal[F]): F[A] = {
    val timeoutEffect = temporal.sleep(duration)
    val result = temporal.race(fa, timeoutEffect)

    result.flatMap {
      case Left(v) => temporal.pure(v)
      case Right(_) => temporal.raiseError(new RuntimeException("Computation timed out."))
    }
  }

  override def run = ???
}