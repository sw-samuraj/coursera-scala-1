package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    *  - test
    *  - ignore
    *  - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")

      assert(!contains(s1, 0), "Singleton")
      assert(!contains(s1, 2), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }

    new TestSets {
      val s = union(s2, s3)
      assert(!contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(contains(s, 3), "Union 3")
    }

    new TestSets {
      val s = union(s1, s3)
      assert(contains(s, 1), "Union 1")
      assert(!contains(s, 2), "Union 2")
      assert(contains(s, 3), "Union 3")
    }
  }

  trait TestDoubletonSets {
    val d1 = doubletonSet(1, 2)
    val d2 = doubletonSet(2, 3)
    val d3 = doubletonSet(1, 3)
  }

  test("intersect contains only common elements") {
    new TestSets {
      val i = intersect(s1, s2)
      assert(!contains(i, 1))
    }
    new TestSets {
      val i = intersect(s1, s1)
      assert(contains(i, 1))
    }
    new TestDoubletonSets {
      val i = intersect(d1, d2)
      assert(!contains(i, 1))
      assert(contains(i, 2))
      assert(!contains(i, 3))
    }
    new TestDoubletonSets {
      val i = intersect(d2, d3)
      assert(!contains(i, 1))
      assert(!contains(i, 2))
      assert(contains(i, 3))
    }
    new TestDoubletonSets {
      val i = intersect(d1, d3)
      assert(contains(i, 1))
      assert(!contains(i, 2))
      assert(!contains(i, 3))
    }
  }

  test("diff contains only elements from s which aren't in t") {
    new TestSets {
      val d = diff(s1, s2)
      assert(contains(d, 1))
    }
    new TestSets {
      val d = diff(s1, s1)
      assert(!contains(d, 1))
    }
    new TestDoubletonSets {
      val d = diff(d1, d2)
      assert(contains(d, 1))
      assert(!contains(d, 2))
      assert(!contains(d, 3))
    }
    new TestDoubletonSets {
      val d = diff(d2, d3)
      assert(!contains(d, 1))
      assert(contains(d, 2))
      assert(!contains(d, 3))
    }
    new TestDoubletonSets {
      val d = diff(d1, d3)
      assert(!contains(d, 1))
      assert(contains(d, 2))
      assert(!contains(d, 3))
    }
  }

  test("odd numbers") {
    assert(!odd(0))
    assert(odd(1))
    assert(!odd(2))
    assert(odd(3))
    assert(!odd(4))
    assert(odd(5))
    assert(!odd(6))
  }

  test("even numbers") {
    assert(even(0))
    assert(!even(1))
    assert(even(2))
    assert(!even(3))
    assert(even(4))
    assert(!even(5))
    assert(even(6))
  }

  trait TestRangeSets {
    val r1 = rangeSet(1, 10)
    val r2 = rangeSet(-10, -1)
  }

  test("filter filters the set via a predicate") {
    new TestDoubletonSets {
      val f = filter(d1, odd)
      assert(contains(f, 1))
      assert(!contains(f, 2))
    }
    new TestDoubletonSets {
      val f = filter(d1, even)
      assert(!contains(f, 1))
      assert(contains(f, 2))
    }
    new TestRangeSets {
      val f = filter(r1, odd)
      assert(!contains(f, 0))
      assert(contains(f, 1))
      assert(!contains(f, 2))
      assert(contains(f, 3))
      assert(!contains(f, 4))
      assert(contains(f, 5))
      assert(!contains(f, 6))
      assert(contains(f, 7))
      assert(!contains(f, 8))
      assert(contains(f, 9))
      assert(!contains(f, 10))
      assert(!contains(f, 11))
    }
    new TestRangeSets {
      val f = filter(r1, even)
      assert(!contains(f, 0))
      assert(!contains(f, 1))
      assert(contains(f, 2))
      assert(!contains(f, 3))
      assert(contains(f, 4))
      assert(!contains(f, 5))
      assert(contains(f, 6))
      assert(!contains(f, 7))
      assert(contains(f, 8))
      assert(!contains(f, 9))
      assert(contains(f, 10))
      assert(!contains(f, 11))
    }
  }

  test("positive numbers") {
    assert(!pos(-1))
    assert(!pos(0))
    assert(pos(1))
  }

  test("negative numbers") {
    assert(neg(-1))
    assert(!neg(0))
    assert(!neg(1))
  }

  test("forall existential quantifier") {
    new TestRangeSets {
      assert(forall(r1, pos))
      assert(forall(r2, neg))
      assert(!forall(r1, neg))
      assert(!forall(r2, pos))
      assert(forall(filter(r1, even), even))
      assert(forall(filter(r1, odd), odd))
    }
  }

  test("exists existential quantifier") {
    new TestRangeSets {
      assert(exists(r1, pos))
      assert(exists(r2, neg))
      assert(!exists(r1, neg))
      assert(!exists(r2, pos))
      assert(exists(union(r1, r2), pos))
      assert(exists(union(r1, r2), neg))
    }
  }

  test("map sequence function") {
    val r = rangeSet(1, 3)
    val doubledSet = map(r, (x: Int) => x * 2)
    val squaredSet = map(r, (x: Int) => x * x)

    assert(contains(doubledSet, 2))
    assert(contains(doubledSet, 4))
    assert(contains(doubledSet, 6))

    assert(contains(squaredSet, 1))
    assert(contains(squaredSet, 4))
    assert(contains(squaredSet, 9))
  }

}
