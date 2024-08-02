package com.corem.part2effects

import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple:

  import scala.concurrent.ExecutionContext.Implicits.global
  def heavyComputation(string: String): Future[Int] =
    Future:
      Thread.sleep(Random.nextInt(1000))
      string.split(" ").length

  val workLoad: List[String] =
    List(
      "I like CE",
      "Scala is great",
      "Looking forward to some awesome stuff"
    )
  def clunkyFuture(): Unit =
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)

  import cats.instances.list._
  import cats.Traverse
  val listTraverse = Traverse[List]

  def traverseFuture(): Unit =
    val singleFuture: Future[List[Int]] =
      listTraverse.traverse(workLoad)(heavyComputation)

    singleFuture.foreach(println)

  import com.corem.utils._

  // Traverse for IO
  def computeAsIO(string: String): IO[Int] =
    IO:
      Thread.sleep(Random.nextInt(1000))
      string.split(" ").length
    .myDebug

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // Parallel traversal
  import cats.syntax.parallel._ // parTraverse extension method
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /*
    Exercices:
    - 1.
    - 2.
   */

  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)(identity)

  def sequenceV2[F[_]: Traverse, A](containerOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(containerOfIOs)(identity)

  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(identity)

  def parSequenceV2[F[_]: Traverse, A](containerOfIOs: F[IO[A]]): IO[F[A]] =
    containerOfIOs.parTraverse(identity)

  // Existing API:
  val singleIOV2: IO[List[Int]] = listTraverse.sequence(ios)
  val parallelSingleIOV2: IO[List[Int]] = parSequence(ios)
  val parallelSingleIOV3: IO[List[Int]] = ios.parSequence

  override def run: IO[Unit] =
    parallelSingleIOV3.map(_.sum).myDebug.void
