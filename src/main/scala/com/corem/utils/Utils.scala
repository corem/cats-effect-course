package com.corem.utils

import cats.effect.IO

extension [A](io: IO[A])
  def myDebug: IO[A] =
    for
      a <- io
      t = Thread.currentThread().getName
      _ = println(s"[$t] $a")
    yield a
