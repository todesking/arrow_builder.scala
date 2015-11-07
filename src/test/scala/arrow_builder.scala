import scalaz.Arrow
import scalaz.syntax.arrow._

import org.scalatest.{ FunSpec, Matchers }

trait TestArrow[A, B] {
  def apply(a: A): B
  def asBaseType: TestArrow[A, B] = this
  def name(name: String): TestArrow[A, B] =
    TestArrow.Name(name, this)
}
object TestArrow {
  implicit val arrowInstance = new Arrow[TestArrow] {
    override def arr[A, B](f: A => B): TestArrow[A, B] = Fun(f)
    override def first[A, B, C](ab: TestArrow[A, B]): TestArrow[(A, C), (B, C)] = First[A, B, C](ab)
    override def second[A, B, C](ab: TestArrow[A, B]): TestArrow[(C, A), (C, B)] = Second[A, B, C](ab)
    override def id[A]: TestArrow[A, A] = Id()
    override def compose[A, B, C](bc: TestArrow[B, C], ab: TestArrow[A, B]): TestArrow[A, C] = Compose(bc, ab)
  }

  case class Fun[A, B](f: A => B) extends TestArrow[A, B] {
    override def apply(a: A) = f(a)
  }

  case class First[A, B, C](ab: TestArrow[A, B]) extends TestArrow[(A, C), (B, C)] {
    override def apply(ac: (A, C)) = (ab.apply(ac._1) -> ac._2)
  }

  case class Second[A, B, C](ab: TestArrow[A, B]) extends TestArrow[(C, A), (C, B)] {
    override def apply(ca: (C, A)) = (ca._1 -> ab.apply(ca._2))
  }

  case class Id[A, B]()(implicit ev: A =:= B) extends TestArrow[A, B] {
    override def apply(a: A) = a
  }

  case class Compose[A, B, C](bc: TestArrow[B, C], ab: TestArrow[A, B]) extends TestArrow[A, C] {
    override def apply(a: A) = bc.apply(ab.apply(a))
  }

  case class Name[A, B](name: String, base: TestArrow[A, B]) extends TestArrow[A, B] {
    override def apply(a: A) = base.apply(a)
  }
}

class Test extends FunSpec with Matchers {
  import com.todesking.arrow_builder.ArrowBuilder
  import com.todesking.arrow_builder.ArrowSyntax._
  import TestArrow._

  describe("ArrowBuilder") {
    it("should build identity arrow") {
      val arrow: TestArrow[Int, Int] = ArrowBuilder.build[TestArrow, Int, Int] { in => in.through }
      arrow should be(Id[Int, Int]())
      arrow(1) should be(1)
    }
    it("should build Fun with Signal.map") {
      val arrow: TestArrow[Int, String] = ArrowBuilder.build[TestArrow, Int, String] { in =>
        in.map(_.toString).through
      }
      arrow match {
        case Compose(Fun(_), Id()) => // ok
      }
      arrow(1) should be("1")
    }
    it("should build Fun with -<") {
      val arrow: TestArrow[Int, String] = ArrowBuilder.build[TestArrow, Int, String] { in =>
        Fun[Int, String](_.toString).asBaseType -< in
      }
      arrow match {
        case Compose(Fun(_), Id()) => // ok
      }
      arrow(1) should be("1")
    }
    it("should build Fun with for comprehension") {
      val arrow: TestArrow[Int, String] = ArrowBuilder.build[TestArrow, Int, String] { in =>
        for {
          out <- Fun[Int, String](_.toString).asBaseType -< in
        } yield out
      }
      arrow match {
        case Compose(Fun(_), Id()) => // ok
      }
      arrow(1) should be("1")
    }
    it("should build Fun with for comprehension: Unused signals is ignored") {
      val arrow: TestArrow[Int, String] = ArrowBuilder.build[TestArrow, Int, String] { in =>
        for {
          unused1 <- Fun[Int, Int] { _ + 10 }.asBaseType -< in
          out <- Fun[Int, String](_.toString).asBaseType -< in
          unused2 <- Fun[String, Int] { _.toInt }.asBaseType -< out
        } yield out
      }
      arrow match {
        case Compose(Fun(_), Id()) => // ok
      }
      arrow(1) should be("1")
    }
    it("should build complex Arrow") {
      val arrow: TestArrow[Int, Int] = ArrowBuilder.build[TestArrow, Int, Int] { in =>
        for {
          plus1 <- Fun[Int, Int](_ + 1).name("plus1") -< in
          times10 <- Fun[Int, Int](_ * 10).name("times10") -< in
          plus1_plus3 <- Fun[Int, Int](_ + 3).name("plus1_plus3") -< plus1
          times10_plus_plus1 <- (times10 + plus1).through
          out <- Fun[(Int, Int), Int] { case (a, b) => a + b }.name("out") -< (plus1_plus3 -> times10)
        } yield out
      }
      arrow match {
        case Compose(Name("out", _), _) => // TODO
      }
      arrow(1) should be(15)
    }
  }
}
