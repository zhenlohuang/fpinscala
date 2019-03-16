package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers {
  "Exercises 3.25" should "pass" in {
    Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 5
  }

  "Exercises 3.26" should "pass" in {
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 3
  }

  "Exercises 3.27" should "pass" in {
    Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 2
  }

  "Exercises 3.28" should "pass" in {
    Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))((x) => x + 1) shouldBe Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))
  }
}
