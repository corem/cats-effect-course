package com.corem.part4coordination

import cats.effect
import cats.effect.kernel.Deferred
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Ref, Concurrent}

import scala.util.Random
import scala.concurrent.duration._
import com.corem.utils._
import cats.syntax.parallel._

import scala.collection.immutable.Queue

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.syntax.monadCancel._

abstract class Mutex {
  def acquire: IO[Unit]
  def release: IO[Unit]
}

object Mutex {
  type Signal = Deferred[IO, Unit]
  case class State(locked: Boolean, waiting: Queue[Signal])
  val unlocked = State(locked = false, Queue())

  def createSignal(): IO[Signal] = Deferred[IO, Unit]

  def create: IO[Mutex] = Ref[IO].of(unlocked).map(createMutexWithCancellation)

  def createMutexWithCancellation(state: Ref[IO, State]): Mutex =
    new Mutex {
      override def acquire = IO.uncancelable { poll =>
        createSignal().flatMap { signal =>

          val cleanup = state.modify {
            case State(locked, queue) =>
              val newQueue = queue.filterNot(_ eq signal)
              State(locked, newQueue) -> release
          }.flatten

          state.modify {
            case State(false, _) => State(locked = true, Queue()) -> IO.unit
            case State(true, queue) => State(locked = true, queue.enqueue(signal)) -> poll(signal.get).onCancel(cleanup)
          }.flatten
        }
      }

      override def release = state.modify {
        case State(false, _) => unlocked -> IO.unit
        case State(true, queue) =>
          if (queue.isEmpty) unlocked -> IO.unit
          else {
            val (signal, rest) = queue.dequeue
            State(locked = true, rest) -> signal.complete(()).void
          }
      }.flatten
    }

  def createSimpleMutex(state: Ref[IO, State]): Mutex = new Mutex {
    override def acquire = createSignal().flatMap { signal =>
      state.modify {
        case State(false, _) => State(locked = true, Queue()) -> IO.unit
        case State(true, queue) => State(locked = true, queue.enqueue(signal)) -> signal.get
      }.flatten
    }

    override def release = state.modify {
      case State(false, _) => unlocked -> IO.unit
      case State(true, queue) =>
        if (queue.isEmpty) unlocked -> IO.unit
        else {
          val (signal, rest) = queue.dequeue
          State(locked = true, rest) -> signal.complete(()).void
        }
    }.flatten
  }
}

abstract class MutexV2[F[_]] {
  def acquire: F[Unit]
  def release: F[Unit]
}

object MutexV2 {
  type Signal[F[_]] = Deferred[F, Unit]
  case class State[F[_]](locked: Boolean, waiting: Queue[Signal[F]])

  def unlocked[F[_]] = State[F](locked = false, Queue())
  def createSignal[F[_]](using concurrent: Concurrent[F]): F[Signal[F]] = concurrent.deferred[Unit]

  def create[F[_]](using concurrent: Concurrent[F]): F[MutexV2[F]] =
    concurrent.ref(unlocked).map(initialState => createMutexWithCancellation(initialState))

  def createMutexWithCancellation[F[_]](state: Ref[F, State[F]])(using concurrent: Concurrent[F]): MutexV2[F] =
    new MutexV2[F] {
      override def acquire = concurrent.uncancelable { poll =>
        createSignal.flatMap { signal =>

          val cleanup = state.modify {
            case State(locked, queue) =>
              val newQueue = queue.filterNot(_ eq signal)
              State(locked, newQueue) -> release
          }.flatten

          state.modify {
            case State(false, _) => State[F](locked = true, Queue()) -> concurrent.unit
            case State(true, queue) => State[F](locked = true, queue.enqueue(signal)) -> poll(signal.get).onCancel(cleanup)
          }.flatten
        }
      }

      override def release = state.modify {
        case State(false, _) => unlocked[F] -> concurrent.unit
        case State(true, queue) =>
          if (queue.isEmpty) unlocked[F] -> concurrent.unit
          else {
            val (signal, rest) = queue.dequeue
            State[F](locked = true, rest) -> signal.complete(()).void
          }
      }.flatten
    }

}

object MutexPlayground extends IOApp.Simple {

  def criticalTask(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def createNonLockingTask(id: Int): IO[Int] = for {
    _ <- IO(s"[Task $id] Working...").myDebug
    res <- criticalTask()
    _ <- IO(s"[Task $id] Got result: $res").myDebug
  } yield res

  def demoNonLockingTasks(): IO[List[Int]] = (1 to 10).toList.parTraverse(id => createNonLockingTask(id))

  def createLockingTask(id: Int, mutex: MutexV2[IO]): IO[Int] = for {
    _ <- IO(s"[Task $id] Waiting for permission...").myDebug
    _ <- mutex.acquire
    _ <- IO(s"[Task $id] Working...").myDebug
    res <- criticalTask()
    _ <- IO(s"[Task $id] Got result: $res").myDebug
    _ <- mutex.release
    _ <- IO(s"[Task $id] Lock removed.").myDebug
  } yield res

  def demoLockingTasks() = for {
    mutex <- MutexV2.create[IO]
    results <- (1 to 10).toList.parTraverse(id => createLockingTask(id, mutex))
  } yield results
  
  def createCancellingTask(id: Int, mutex: MutexV2[IO]): IO[Int] = {
    if (id % 2 == 0) createLockingTask(id, mutex)
    else for {
      fib <- createLockingTask(id, mutex).onCancel(IO(s"[Task $id] Received cancellation!").myDebug.void).start
      _ <- IO.sleep(2.seconds) >> fib.cancel
      out <- fib.join
      result <- out match {
        case Succeeded(effect) => effect
        case Errored(_) => IO(-1)
        case Canceled() => IO(-2)
      }
    } yield result
  }

  def demoCancellingTasks() = for {
    mutex <- MutexV2.create[IO]
    results <- (1 to 10).toList.parTraverse(id => createCancellingTask(id, mutex))
  } yield results

  override def run = demoCancellingTasks().myDebug.void
}