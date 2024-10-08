package com.corem.part3fibers

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Resource}
import com.corem.utils.*

import java.io.*
import java.util.Scanner
import scala.concurrent.duration.*

object Resources extends IOApp.Simple:

  class Connection(url: String):
    def open(): IO[String] = IO(s"Opening connection to $url").myDebug
    def close(): IO[String] = IO(s"Closing connection to $url").myDebug

  val asyncFetchUrl =
    for
      fib <- (Connection("rockthejvm.com").open() *> IO.sleep(
        (Int.MaxValue).seconds
      )).start
      _ <- IO.sleep(1.second) *> fib.cancel
    yield () // Problem: leaking resources

  val correctAsyncFetchUrl =
    for
      conn <- IO(Connection("rockthejvm.com"))
      fib <- (conn.open() *> IO.sleep((Int.MaxValue).seconds))
        .onCancel(conn.close().void)
        .start
      _ <- IO.sleep(1.second) *> fib.cancel
    yield ()

  val bracketFetchUrl = IO(Connection("rockthejvm.com"))
    .bracket(conn => conn.open() *> IO.sleep(Int.MaxValue.seconds))(conn =>
      conn.close().void
    )

  val bracketProgram =
    for
      fib <- bracketFetchUrl.start
      _ <- IO.sleep(1.second) *> fib.cancel
    yield ()

  /*
    Exercices:
    -
   */
  def openFileScanner(path: String): IO[Scanner] =
    IO(Scanner(FileReader(File(path))))

  def readLineByLine(scanner: Scanner): IO[Unit] =
    if scanner.hasNextLine then
      IO(scanner.nextLine()).myDebug >> IO.sleep(50.millis) >> readLineByLine(
        scanner
      )
    else IO.unit

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"Opening file at $path") >>
      openFileScanner(path).bracket { scanner =>
        readLineByLine(scanner)
      } { scanner =>
        IO("Closing file at $path").myDebug >> IO(scanner.close())
      }

  /*
    Resources
   */
  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path)
      .bracket { scanner =>
        IO(Connection(scanner.nextLine())).bracket { conn =>
          conn.open().myDebug >> IO.never
        }(conn => conn.close().myDebug.void)
      }(scanner => IO("Closing file").myDebug >> IO(scanner.close()))

  val connectionResource =
    Resource.make(IO(Connection("rockthejvm.com")))(conn => conn.close().void)
  // ... at a later part of your code

  val resourceFetchUrl =
    for
      fib <- connectionResource.use(conn => conn.open() >> IO.never).start
      _ <- IO.sleep(1.second) >> fib.cancel
    yield ()

  val simpleResource = IO("String")
  val usingResource: String => IO[String] = string =>
    IO(s"Using the string: $string").myDebug
  val releaseResource: String => IO[Unit] = string =>
    IO(s"Finalizing the string: $string").myDebug.void

  val usingResourceWithBracket =
    simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource =
    Resource.make(simpleResource)(releaseResource).use(usingResource)

  /*
    Exercice
      - Try to read a file with one line every 100 millis, using Resource
      - (Refactor the bracket exercice to use Resource)
   */

  def getResourceFromFile(path: String) = Resource.make(openFileScanner(path)) {
    scanner =>
      IO(s"Closing file at $path").myDebug >> IO(scanner.close())
  }

  def resourceReadFile(path: String) =
    IO(s"Opening file at $path") >> getResourceFromFile(path).use { scanner =>
      readLineByLine(scanner)
    }

  def cancelReadFile(path: String) =
    for
      fib <- resourceReadFile(path).start
      _ <- IO.sleep(1.seconds) >> fib.cancel
    yield ()

  // Nested resources
  def connectionFromConfResource(path: String) =
    Resource
      .make(IO("Opening file").myDebug >> openFileScanner(path))(scanner =>
        IO(s"Closing file at $path").myDebug >> IO(scanner.close())
      )
      .flatMap(scanner =>
        Resource.make(IO(Connection(scanner.nextLine())))(conn =>
          conn.close().void
        )
      )

  def connFromConfResourceClean(path: String) =
    for
      scanner <- Resource
        .make(IO("Opening file").myDebug >> openFileScanner(path))(scanner =>
          IO(s"Closing file at $path").myDebug >> IO(scanner.close())
        )
      conn <- Resource.make(IO(Connection(scanner.nextLine())))(conn =>
        conn.close().void
      )
    yield conn

  // Connection + file will close automatically
  val openConnection = connFromConfResourceClean(
    "src/main/resources/connection.txt"
  ).use(conn => conn.open() >> IO.never)

  val canceledConnection =
    for
      fib <- openConnection.start
      _ <- IO.sleep(1.second) >> IO("Cancelling").myDebug >> fib.cancel
    yield ()

  // Finalizers to regular IOs
  val ioWithFinalizer =
    IO("Some resource").myDebug.guarantee(IO("Freeing resource").myDebug.void)

  val ioWithFinalizerV2 =
    IO("Some resource").myDebug.guaranteeCase {
      case Succeeded(fa) =>
        fa.flatMap(result => IO(s"Releasing resource: $result").myDebug).void
      case Errored(e) => IO("Nothing to release").myDebug.void
      case Canceled() =>
        IO("Resource got canceled, releasing what's left").myDebug.void
    }

  //  override def run: IO[Unit] = bracketReadFile("src/main/scala/com/corem/part3fibers/Resources.scala").void
  //  override def run: IO[Unit] = resourceReadFile(
  //    "src/main/scala/com/corem/part3fibers/Resources.scala"
  //  ).void
  //  override def run: IO[Unit] = cancelReadFile(
  //    "src/main/scala/com/corem/part3fibers/Resources.scala"
  //  )
//  override def run: IO[Unit] = canceledConnection
  override def run: IO[Unit] = ioWithFinalizerV2.void
