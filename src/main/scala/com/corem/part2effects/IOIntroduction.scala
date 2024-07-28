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

   */

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
//    println(aDelayedIO.unsafeRunSync())
//    println(smallProgram.unsafeRunSync())
    println(smallProgramV2.unsafeRunSync())
