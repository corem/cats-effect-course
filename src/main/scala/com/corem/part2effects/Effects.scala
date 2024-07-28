package com.corem.part2effects

import scala.concurrent.Future
import scala.io.StdIn

object Effects:

  // Pure functional programming
  def combine(a: Int, b: Int): Int = a + b
  // Referential transparency:
  val aSum = combine(2, 3)
  val aSumV2 = 2 + 3
  val aSumV3 = 5

  // Side effect example: print to the console
  val printSomething: Unit = println("Cats Effect")
  val printSomethingV2: Unit = () // Not identical

  // Change a variable
  var anInt = 0
  val changingVar: Unit = (anInt + 1)
  val changingVarV2: Unit = () // Not identical

  // Still, side effects are necessary

  // Effect - data type
  /*
    Properties:
    - type signature describes the kind of calculation that will be performed
    - type signature describes the value that will be calculated
    - when side effects are needed, effect construction is separate from effect execution
   */

  /*
    Example Option = possibly absent value
    - describes a possibly absent value
    - computes a value of type A, if it exists
    - side effects are not needed
   */
  val anOption: Option[Int] = Option(42)

  /*
    Example Future =
    - Describes an asynchronous computation
    - Computes a value of type A, if it's successful
    - Side effect is required (allocating/scheduling a thread), execution is NOT separate from construction
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /*
    Example MyIO
    - describes any computation
    - calculates a value of type A if it's successful
    - side effects are required for the evaluation of () => A
   */
  case class MyIO[A](unsafeRun: () => A):
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something...")
    42
  })

  /*
    Exercices
    - 1. An IO which returns the current time of the system
    - 2. An IO which measures the duration of a computation
    - 3. An IO which print something to the console
    - 4. An IO which reads a line (str) from the std input
   */

  // 1
  val clock: MyIO[Long] =
    MyIO(() => System.currentTimeMillis())

  // 2
  def measure[A](computation: MyIO[A]): MyIO[Long] =
    for
      start <- clock
      _ <- computation
      end <- clock
    yield end - start

  val mySleepIO: MyIO[Unit] =
    MyIO(() => Thread.sleep(1000))

  def testTimeIO(): Unit =
    val test = measure(mySleepIO)
    println(test.unsafeRun())

  // 3
  def myPrint(text: String): MyIO[Unit] =
    MyIO(() => println(text))

  // 4
  val myRead: MyIO[String] =
    MyIO(() => StdIn.readLine())

  def testConsole(): Unit =
    val program =
      for
        _ <- myPrint("Enter your name: ")
        name <- myRead
        _ <- myPrint(s"Welcome $name")
      yield ()

    program.unsafeRun()

  def main(args: Array[String]): Unit =
    testTimeIO()
    testConsole()
