package com.corem.part2effects

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object IOErrorHandling:

  val aFaildCompute: IO[Int] = IO(throw new RuntimeException("A Failure"))
  val aProperFail: IO[Int] =
    IO.raiseError(new RuntimeException("A Proper Failure"))

  // Handle exceptions
  val dealWithIt = aProperFail.handleErrorWith { case _: RuntimeException =>
    IO.delay(println("I'm still here"))
  }

  // Use Either
  val effectAsEither: IO[Either[Throwable, Int]] = aProperFail.attempt

  // Redeem: transform the failure and the success in one go
  val resultAsString: IO[String] =
    aProperFail.redeem(ex => s"Fail: $ex", value => s"Success: $value")

  // RedeemWith
  val resultAsEffect = aProperFail.redeemWith(
    ex => IO(println(s"Fail: $ex")),
    value => IO(println(s"Success: $value"))
  )

  /*
    Exercices:
    - 1. Construct potentially failed IOs from standard data types (Option, Try, Either)
    - 2. handleError, handleErrorWith
   */

  def optionToIO[A](option: Option[A])(ifEmpty: Throwable): IO[A] =
    option match
      case Some(a) => IO(a)
      case None    => IO.raiseError(ifEmpty)

  def tryToIO[A](aTry: Try[A]): IO[A] =
    aTry match
      case Success(a)         => IO(a)
      case Failure(exception) => IO.raiseError(exception)
  def eitherToIO[A](anEither: Either[Throwable, A]): IO[A] =
    anEither match
      case Left(value)  => IO.raiseError(value)
      case Right(value) => IO(value)

  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.redeem(ex => handler(ex), identity)
  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.redeemWith(handler, IO.pure)

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    resultAsEffect.unsafeRunSync()
