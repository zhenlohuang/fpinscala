package fpinscala.gettingstarted

import org.scalatest.{FlatSpec, Matchers}

class GettingStartedTest extends FlatSpec with Matchers {
  "Exercises 2.1" should "pass" in {
    MyModule.fib(5) shouldBe 5
  }

  "Exercises 2.2" should "pass" in {
    PolymorphicFunctions.isSorted(Array(1, 3, 5, 7), (x: Int, y: Int) => x > y) shouldBe true
    PolymorphicFunctions.isSorted(Array(7, 5, 1, 3), (x: Int, y: Int) => x < y) shouldBe false
  }

  "Exercises 2.3" should "pass" in {
    def f(a: Int, b: Int): Int = a + b
    def g(a: Int)(b: Int): Int = a + b

    PolymorphicFunctions.curry(f)(1)(1) == f(1, 1) shouldBe true
    PolymorphicFunctions.curry(f)(1)(1) == g(1)(1) shouldBe true
  }

  "Exercises 2.4" should "pass" in {
    def f(a: Int, b: Int): Int = a + b
    def g(a: Int)(b: Int): Int = a + b

    PolymorphicFunctions.uncurry(g)(1, 1) == g(1)(1) shouldBe true
    PolymorphicFunctions.uncurry(g)(1, 1) == f(1, 1) shouldBe true
  }

  "Exercises 2.5" should "pass" in {
    def f(b: Int): Int = b / 2
    def g(a: Int): Int = a + 2

    PolymorphicFunctions.compose(f, g)(0) == PolymorphicFunctions.compose(g, f)(0) shouldBe false
    PolymorphicFunctions.compose(f, g)(2) shouldBe 2
    PolymorphicFunctions.compose(g, f)(2) shouldBe 3
  }
}
