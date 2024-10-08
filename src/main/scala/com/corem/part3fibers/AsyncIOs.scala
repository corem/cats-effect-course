package com.corem.part3fibers

import cats.effect.{IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object AsyncIOs extends IOApp.Simple {

  import com.corem.utils._

  val threadPool = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit

  def computeMeaningOfLife(): Int = {
    Thread.sleep(1000)
    println(
      s"[${Thread.currentThread().getName}] computing the meaning of life on some other thread..."
    )
    42
  }

  def computeMeaningOfLifeEither(): Either[Throwable, Int] = Try {
    computeMeaningOfLife()
  }.toEither

  def computeMolOnThreadPool(): Unit =
    threadPool.execute(() => computeMeaningOfLife())

  val asyncMolIO: IO[Int] = IO.async_ { (cb: Callback[Int]) =>
    threadPool.execute { () =>
      val result = computeMeaningOfLifeEither()
      cb(result)
    }
  }

  /*
    Exercise: lift an async computation on ec to an IO
   */
  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_[A] { (cb: Callback[A]) =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        cb(result)
      }
    }

  val asyncMolIO_v2: IO[Int] = asyncToIO(computeMeaningOfLife)(ec)

  /*
  Exercise: lift an async computation as a Future to an IO
   */
  def convertFutureToIO[A](future: => Future[A]): IO[A] =
    IO.async_ { (cb: Callback[A]) =>
      future.onComplete { tryResult =>
        val result = tryResult.toEither
        cb(result)
      }
    }

  lazy val molFuture: Future[Int] = Future {
    computeMeaningOfLife()
  }
  val asyncMolIO_v3: IO[Int] = convertFutureToIO(molFuture)
  val asyncMolIO_v4: IO[Int] = IO.fromFuture(IO(molFuture))

  /*
  Exercise: a never-ending IO?
   */
  val neverEndingIO: IO[Int] = IO.async_[Int](_ => ())
  val neverEndingIO_v2: IO[Int] = IO.never

  import scala.concurrent.duration._

  def demoAsyncCancellation() = {
    val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async { (cb: Callback[Int]) =>
      IO {
        threadPool.execute { () =>
          val result = computeMeaningOfLifeEither()
          cb(result)
        }
      }.as(Some(IO("Cancelled").myDebug.void))
    }

    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _ <- IO.sleep(500.millis) >> IO("Cancelling...").myDebug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run =
    demoAsyncCancellation().myDebug >> IO(threadPool.shutdown())
}
