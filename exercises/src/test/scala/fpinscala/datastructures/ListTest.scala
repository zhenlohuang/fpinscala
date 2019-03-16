package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {
  "Exercises 3.1" should "pass" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    x shouldBe 3
  }

  "Exercises 3.2" should "pass" in {
    List.tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  "Exercises 3.3" should "pass" in {
    List.setHead(List(1, 2, 3), 0) shouldBe List(0, 2, 3)
  }

  "Exercises 3.4" should "pass" in {
    List.drop(List(1, 2, 3), 2) shouldBe List(3)
  }

  "Exercises 3.5" should "pass" in {
    List.dropWhile(List(1, 2, 3), (x: Int) => x < 3) shouldBe List(3)
  }

  "Exercises 3.6" should "pass" in {
    List.init(List(1, 2, 3)) shouldBe List(1, 2)
  }

  "Exercises 3.9" should "pass" in {
    List.length(List(1, 2, 3)) shouldBe 3
  }

  "Exercises 3.10" should "pass" in {
    List.foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
  }

  "Exercises 3.15" should "pass" in {
    List.concat(List(List(1, 2), List(2, 3))) shouldBe List(1, 2, 2, 3)
  }

  "Exercises 3.16" should "pass" in {
    List.add1(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  "Exercises 3.17" should "pass" in {
    List.doubleToString(List(1.0, 2.0, 3.0)) shouldBe List("1.0", "2.0", "3.0")
  }

  "Exercises 3.18" should "pass" in {
    List.map(List(1, 2, 3))((x: Int) => x + 1) shouldBe List(2, 3, 4)
  }

  "Exercises 3.19" should "pass" in {
    List.filter(List(1, 2, 3))((x: Int) => x % 2 == 0) shouldBe List(2)
  }

  "Exercises 3.20" should "pass" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  "Exercises 3.21" should "pass" in {
    List.filterViaFlatMap(List(1, 2, 3))((x: Int) => x % 2 == 0) shouldBe List(2)
  }

  "Exercises 3.22" should "pass" in {
    List.addPairwise(List(1, 2, 3), List(2, 3, 4)) shouldBe List(3, 5, 7)
  }

  "Exercises 3.23" should "pass" in {
    List.zipWith(List(1, 2, 3), List(2, 3, 4))(_ + _) shouldBe List(3, 5, 7)
  }

  "Exercises 3.24" should "pass" in {
    List.hasSubsequence(List(1, 2, 3), List(1, 2)) shouldBe true
  }
}
