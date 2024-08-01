package com.corem.part2effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction:

  val ourFirstIO: IO[Int] = IO.pure(42)
  val aDelayedIO: IO[Int] = IO.delay({
    println("Producing an integer")
    54
  })

//  val shouldNotDotThis: IO[Int] = IO.pure({
//    println("Producing an integer")
//    54
//  })

  val aDelayedIOV2: IO[Int] = IO { // apply = delay
    println("Producing an integer")
    54
  }

  // map, flatMap
  val improvedMeaningOfLife = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  val smallProgram: IO[Unit] =
    for
      line1 <- IO(StdIn.readLine())
      line2 <- IO(StdIn.readLine())
      _ <- IO(println(line1 + line2))
    yield ()

  // mapN - combine IO effects as tuples
  import cats.syntax.apply.*
  val combinedMeaningOfLife = (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)
  def smallProgramV2: IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /*
    Exercices:
    - 1. Sequence two IOs and take the result of the last one
    - 2. Sequence two IOs and take the result of the first one
    - 3. Repeat an IO forever
    - 4. Convert an IO to a different type
    - 5. Discard value inside an IO, just return Unit
    - 6. Fix stack recursion
    - 7. Write a fibonacci IO that does not crash on recursion
   */

  def sequenceTakeLast[A,B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  def sequenceTakeLastV2[A,B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob

  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a =>iob.map(_ => a))

  def sequenceTakeFirstV2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io))

  def foreverV2[A](io: IO[A]): IO[A] =
    io >> foreverV2(io)

  def foreverV3[A](io: IO[A]): IO[A] =
    io *> foreverV3(io)

  def foreverV4[A](io: IO[A]): IO[A] =
    io.foreverM

  def convert[A, B](io: IO[A], value: B): IO[B] =
    io.map(_ => value)

  def convertV2[A, B](io: IO[A], value: B): IO[B] =
    io.as(value)

  def asUnit[A](io: IO[A]): IO[Unit] =
    io.map(_ => ())

  def asUnitV2[A](io: IO[A]): IO[Unit] =
    io.as(()) // Not readable

  def asUnitV3[A](io: IO[A]): IO[Unit] =
    io.void

  def sum(n: Int): Int =
    if n <= 0 then 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if n <= 0 then IO(0)
    else for
      lastNumber <- IO(n)
      prevSum <- sumIO(n - 1)
    yield prevSum + lastNumber

  def fibonacci(n: Int): IO[BigInt] =
    if n < 2 then return IO(1)
    else for
      last <- IO.defer(fibonacci(n - 1))
      prev <- IO.defer(fibonacci(n - 2))
    yield last + prev

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
//    println(aDelayedIO.unsafeRunSync())
//    println(smallProgram.unsafeRunSync())
//    println(smallProgramV2.unsafeRunSync())

//    foreverV3(IO {
//      Thread.sleep(1000)
//      println("Forever!")
//    }).unsafeRunSync()

//    println(sumIO(20000).unsafeRunSync())
//    sum(20000)
    (1 to 100).foreach(i => println(fibonacci(i).unsafeRunSync()))
