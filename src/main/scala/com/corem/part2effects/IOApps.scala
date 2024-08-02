package com.corem.part2effects

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn

object IOApps:
  val program =
    for
      line <- IO(StdIn.readLine())
      - <- IO(println(s"You've just written: $line"))
    yield ()

object TestApp:
  import IOApps.program

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    program.unsafeRunSync()

object FirstCEApp extends IOApp:
  import IOApps.program

  override def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)

object MySimpleApp extends IOApp.Simple:
  import IOApps.program

  override def run = program
