package com.corem.part2effects

import cats.effect.IO

object IOErrorHandling:

  val aFaildCompute: IO[Int] = IO(throw new RuntimeException("A Failure"))
  val aProperFail: IO[Int] = IO.raiseError(new RuntimeException("A Proper Failure"))

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    aProperFail.unsafeRunSync()