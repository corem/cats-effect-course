package com.corem.part4coordination

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}
import cats.syntax.parallel.*
import com.corem.utils.*

import scala.concurrent.duration.*
import scala.util.Random

object Semaphores extends IOApp.Simple {

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2)

  def doWorkWhileLoggedIn(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def login(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[Session $id] Waiting to log in...").myDebug
    _ <- sem.acquire
    _ <- IO("[Session %d] Logged in, working...".format(id)).myDebug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"[Session $id] Done: $res, logging out...").myDebug
    _ <- sem.release
  } yield res

  def demoSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1Fib <- login(1, sem).start
    user2Fib <- login(2, sem).start
    user3Fib <- login(3, sem).start
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  def weightedLogin(id: Int, requiredPermits: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[Session $id] Waiting to log in...").myDebug
    _ <- sem.acquireN(requiredPermits)
    _ <- IO(s"[Session $id] Logged in, working...").myDebug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"[Session $id] Done: $res, logging out...").myDebug
    _ <- sem.releaseN(requiredPermits)
  } yield res

  def demoWeightedSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1Fib <- weightedLogin(1, 1, sem).start
    user2Fib <- weightedLogin(2, 2, sem).start
    user3Fib <- weightedLogin(3, 3, sem).start
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  /*
    Exercise:
    1. Find out if there's something wrong with this code
    2. Why
    3. fix it
   */
  val mutex = Semaphore[IO](1)
  val users: IO[List[Int]] = (1 to 10).toList.parTraverse { id =>
    for {
      sem <- mutex
      _ <- IO(s"[session $id] waiting to log in...").myDebug
      _ <- sem.acquire
      // critical section
      _ <- IO(s"[session $id] logged in, working...").myDebug
      res <- doWorkWhileLoggedIn()
      _ <- IO(s"[session $id] done: $res, logging out...").myDebug
      // end of critical section
      _ <- sem.release
    } yield res
  }

  // 1
  // Expected: all tasks start at the same time, only one can work at one time
  // Reality: all tasks are parallel

  // 2
  // Mistake: we flatMap Semaphore[IO](1) so we create a new semaphore every time

  // 3
  val usersFixed: IO[List[Int]] = mutex.flatMap { sem =>
    (1 to 10).toList.parTraverse { id =>
      for {
        _ <- IO(s"[session $id] waiting to log in...").myDebug
        _ <- sem.acquire
        _ <- IO(s"[session $id] logged in, working...").myDebug
        res <- doWorkWhileLoggedIn()
        _ <- IO(s"[session $id] done: $res, logging out...").myDebug
        _ <- sem.release
      } yield res
    }
  }


  override def run = usersFixed.myDebug.void
}