# Arrow builder for Scala

Poor man's `proc` notation (GHC) for Scala.

## Current status

Very experimental


## Usage

```scala
import com.todesking.arrow_builder.ArrowSyntax._
import com.todesking.arrow_builder.ArrowBuilder

def arr[A, B](f: A => B): MyArrow[A, B] = ???

ArrowBuilder.build[MyArrow, Int, String] { in =>
  a <- arr[Int, Int](_ + 1) -< in
  b <- arr[Int, Int](_ + 2) -< in
  a_b <- arr[(Int, Int), Int] { case (a, b) => a * b } -< a.zip(b)
  s <- arr[Int, String](_.toString) -< a_b
} yield a_b
```

## ArrowDelayLoop

`ArrowLoop` typeclass is not fit to Scala because it requires lazy evaluation.
Instead, we provide `ArrowDelayLoop` typeclass. It is analogue of `ArrowLoop` + `Arrowinit`,
but only provides `delayLoop[A, B, C](init: A, arrow: F[(A, C), (B, C)]): F[A, B]` method.

## rec

Theres no `rec`. Use `ArrowBuilder.delayLoop`
