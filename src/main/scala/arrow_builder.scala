package com.todesking.arrow_builder

import scalaz.Arrow
import scalaz.syntax.arrow._

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.existentials

trait ArrowDelayLoop[F[_, _]] extends Arrow[F] {
  def delayLoop[A, B, C](init: C, a: F[(A, C), (B, C)]): F[A, B]
}

object ArrowSyntax {
  implicit class ArrowExt[F[_, _]: Arrow, A, B](self: F[A, B]) {
    def -<[C](s: Signal[F, C, A]): ArrowBuilder[F, C, B] =
      ArrowBuilder.Bind(self, s)

    def -<[C, D, E](s: (Signal[F, C, D], Signal[F, C, E]))(implicit ev: Signal[F, C, (D, E)] =:= Signal[F, C, A]): ArrowBuilder[F, C, B] = s match {
      case (s1, s2) => self -< s1.zip(s2)
    }
  }
}

// A: root of arrow
// B: destination of arrow
sealed trait ArrowBuilder[F[_, _], A, B] {
  def dest: Signal[F, A, B]

  def map[C](f: Signal[F, A, B] => Signal[F, A, C]): ArrowBuilder[F, A, C] =
    f(this.dest).arrowBuilder

  def flatMap[C](f: Signal[F, A, B] => ArrowBuilder[F, A, C]): ArrowBuilder[F, A, C] =
    f(this.dest)

  def build()(implicit a: Arrow[F]): F[A, B]

  def buildPartial[X](sig: Signal[F, A, X])(implicit a: Arrow[F]): F[X, B]

  def depth: Int

  def signals: Seq[Signal[F, A, _]]

  override def equals(rhs: Any) =
    rhs match {
      case a: AnyRef => this eq a
      case _ => false
    }
}

object ArrowBuilder {
  def build[F[_, _]: Arrow, A, B](f: Signal[F, A, A] => ArrowBuilder[F, A, B]): F[A, B] =
    Start[F, A]().flatMap(f).build()

  def delayLoop[F[_, _]: ArrowDelayLoop, A, B, C](init: C)(f: Signal[F, (A, C), (A, C)] => ArrowBuilder[F, (A, C), (B, C)]): F[A, B] =
    implicitly[ArrowDelayLoop[F]].delayLoop(init, build(f))

  case class Start[F[_, _], A]() extends ArrowBuilder[F, A, A] {
    override val dest = Signal.Dest(this)

    override val depth = 0

    override val signals = Seq(dest)

    override def build()(implicit a: Arrow[F]): F[A, A] = a.id

    override def buildPartial[X](sig: Signal[F, A, X])(implicit a: Arrow[F]): F[X, A] =
      if (sig == dest) a.id[A].asInstanceOf[F[X, A]]
      else throw new IllegalArgumentException()
  }

  case class Bind[F[_, _], A, B, C](arrow: F[B, C], src: Signal[F, A, B]) extends ArrowBuilder[F, A, C] {
    override val dest = Signal.Dest(this)

    override val depth = src.arrowBuilder.depth + 1

    override val signals = dest +: src.arrowBuilder.signals

    override def build()(implicit a: Arrow[F]): F[A, C] =
      src.arrowBuilder.build() >>> arrow

    override def buildPartial[X](sig: Signal[F, A, X])(implicit a: Arrow[F]): F[X, C] =
      if (sig == dest) a.id[A].asInstanceOf[F[X, C]]
      else src.arrowBuilder.buildPartial(sig) >>> arrow
  }
  case class SigMap[F[_, _], A, B, C](ab: ArrowBuilder[F, A, B], f: B => C) extends ArrowBuilder[F, A, C] {
    override val dest = Signal.Dest(this)

    override val depth = ab.depth + 1

    override def signals = dest +: ab.signals

    override def build()(implicit a: Arrow[F]): F[A, C] =
      ab.build() >>> a.arr(f)

    override def buildPartial[X](sig: Signal[F, A, X])(implicit a: Arrow[F]): F[X, C] =
      if (sig == dest) a.id[A].asInstanceOf[F[X, C]]
      else ab.buildPartial(sig) >>> a.arr(f)
  }
  case class Zip[F[_, _], A, B, C](fst: ArrowBuilder[F, A, B], snd: ArrowBuilder[F, A, C]) extends ArrowBuilder[F, A, (B, C)] {
    protected type CommonSignalType

    override val dest = Signal.Dest(this)

    override val depth = math.max(fst.depth, snd.depth) + 1

    override val signals = (fst.signals ++ snd.signals).distinct

    override def build()(implicit a: Arrow[F]): F[A, (B, C)] = {
      def aux[X](): F[A, (B, C)] = {
        val common = commonSignal
        common.arrowBuilder.build() >>> (fst.buildPartial(common) &&& snd.buildPartial(common))
      }
      aux()
    }

    override def buildPartial[X](sig: Signal[F, A, X])(implicit a: Arrow[F]): F[X, (B, C)] =
      if (sig == dest) a.id[A].asInstanceOf[F[X, (B, C)]]
      else fst.buildPartial(sig) &&& snd.buildPartial(sig)

    private[this] def commonSignal: Signal[F, A, CommonSignalType] = {
      fst.signals.sortBy(_.depth).zip(snd.signals.sortBy(_.depth))
        .takeWhile { case (a, b) => a == b }
        .lastOption
        .map(_._1.asInstanceOf[Signal[F, A, CommonSignalType]])
        .getOrElse { throw new IllegalArgumentException() }
    }
  }
}

sealed trait Signal[F[_, _], A, B] {
  def through: ArrowBuilder[F, A, B] = arrowBuilder

  def map[C](f: B => C): Signal[F, A, C] =
    Signal.Map(this, f)

  def zip[C](rhs: Signal[F, A, C]): Signal[F, A, (B, C)] =
    Signal.Zip(this, rhs)

  def arrowBuilder: ArrowBuilder[F, A, B]

  def depth: Int =
    arrowBuilder.depth

  def +(rhs: Signal[F, A, B])(implicit num: Numeric[B]): Signal[F, A, B] =
    zip(rhs).map { case (a, b) => num.plus(a, b) }

  def *(rhs: Signal[F, A, B])(implicit num: Numeric[B]): Signal[F, A, B] =
    zip(rhs).map { case (a, b) => num.times(a, b) }

  override def equals(rhs: Any) =
    rhs match {
      case rhs: Signal[_, _, _] => this.arrowBuilder == rhs.arrowBuilder
      case _ => false
    }

  override def hashCode =
    arrowBuilder.hashCode
}

object Signal {
  case class Start[F[_, _], A](override val arrowBuilder: ArrowBuilder[F, A, A]) extends Signal[F, A, A] {
  }
  case class Dest[F[_, _], A, B](override val arrowBuilder: ArrowBuilder[F, A, B]) extends Signal[F, A, B] {
  }
  case class Map[F[_, _], A, B, C](s: Signal[F, A, B], f: B => C) extends Signal[F, A, C] {
    override val arrowBuilder = ArrowBuilder.SigMap(s.arrowBuilder, f)
  }
  case class Zip[F[_, _], A, B, C](fst: Signal[F, A, B], snd: Signal[F, A, C]) extends Signal[F, A, (B, C)] {
    override val arrowBuilder = ArrowBuilder.Zip(fst.arrowBuilder, snd.arrowBuilder)
  }
}

