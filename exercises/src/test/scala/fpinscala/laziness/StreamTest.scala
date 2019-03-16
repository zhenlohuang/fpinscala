package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {
  "Exercises 5.1" should "pass" in {
    val stream = Stream(1, 2, 3)
    stream.toList() shouldBe List(1, 2, 3)
  }

  "Exercises 5.2" should "pass" in {
    val stream = Stream(1, 2, 3)
    stream.take(2).toList() shouldBe Stream(1, 2).toList()
    stream.drop(2).toList() shouldBe Stream(3).toList()
  }

  "Exercises 5.3" should "pass" in {
    val stream = Stream(1, 2, 3)
    stream.takeWhile((x: Int) => x < 3).toList() shouldBe Stream(1, 2).toList()
  }

  "Exercises 5.4" should "pass" in {
    val stream = Stream(1, 2, 3)
    stream.forAll(_ > 0) shouldBe true
    stream.forAll(_ % 2 == 0) shouldBe false
  }

  "Exercises 5.5" should "pass" in {
    val stream = Stream(1, 2, 3)
    stream.takeWhile2((x: Int) => x < 3).toList() shouldBe Stream(1, 2).toList()
  }

  "Exercises 5.6" should "pass" in {
    val stream = Stream(1, 2, 3)
    stream.headOption shouldBe Some(1)
  }

  "Exercises 5.7" should "pass" in {
    val stream = Stream(1, 2, 3)
    stream.map(_ + 1).toList() shouldBe List(2, 3, 4)
    stream.filter(_ % 2 == 0).toList() shouldBe List(2)
    stream.append(Stream(0, 1)).toList() shouldBe List(1, 2, 3, 0, 1)
    stream.flatMap(x => Stream(x)).toList() shouldBe List(1, 2, 3)
  }

  "Exercises 5.8" should "pass" in {
    Stream.constant(1).take(3).toList() shouldBe List(1, 1, 1)
  }

  "Exercises 5.9" should "pass" in {
    Stream.from(1).take(3).toList() shouldBe List(1, 2, 3)
  }

  "Exercises 5.10" should "pass" in {
    Stream.fib().take(5).toList() shouldBe List(0, 1, 1, 2, 3)
  }
}
