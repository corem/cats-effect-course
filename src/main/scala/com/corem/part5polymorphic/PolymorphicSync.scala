package com.corem.part5polymorphic

import cats.Defer
import cats.effect.{IO, IOApp, MonadCancel, Sync}

import java.io.{BufferedReader, InputStreamReader}

object PolymorphicSync extends IOApp.Simple {

  val aDelayedIO = IO.delay {
    println("I'm an effect!")
    42
  }

  val aBlockingIO = IO.blocking {
    println("loading...")
    Thread.sleep(1000)
    42
  }

  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    def delay[A](thunk: => A): F[A]
    def blocking[A](thunk: => A): F[A]

    def defer[A](thunk: => F[A]): F[A] =
      flatMap(delay(thunk))(identity)
  }

  val syncIO = Sync[IO]

  val aDelayedIO_v2 = syncIO.delay {
    println("I'm an effect!")
    42
  }

  val aBlockingIO_v2 = syncIO.blocking {
    println("loading...")
    Thread.sleep(1000)
    42
  }

  val aDeferredIO = IO.defer(aDelayedIO)

  /*
    Exercise: write a polymorphic console
   */
  trait Console[F[_]] {
    def println[A](a: A): F[Unit]
    def readLine(): F[String]
  }

  import cats.syntax.functor._
  object Console {
    def make[F[_]](using sync: Sync[F]): F[Console[F]] = sync.pure((System.in, System.out)).map {
      case (in, out) => new Console[F] {
        def println[A](a: A): F[Unit] =
          sync.blocking(out.println(a))

        def readLine(): F[String] = {
          val bufferedReader = new BufferedReader(new InputStreamReader(in))
          sync.blocking(bufferedReader.readLine())
        }
      }
    }
  }

  def consoleReader(): IO[Unit] = for {
    console <- Console.make[IO]
    _ <- console.println("Hi, what's your name?")
    name <- console.readLine()
    _ <- console.println(s"Hi $name, nice to meet you!")
  } yield ()

  override def run = consoleReader()
}