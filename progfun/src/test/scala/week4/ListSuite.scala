package week4

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListSuite extends FunSuite{

  trait Fixture {
    val empty = new Nil
    val l = new Cons(1, new Nil)
    val le = List()
    val l1 = List(42)
    val l2 = List(12, 42)
  }

  test("empty list isEmpty returns true") {
    new Fixture {
      assert(empty.isEmpty)
    }
  }

  test("head of empty list throws exception") {
    new Fixture {
      assertThrows[NoSuchElementException] {
        empty.head
      }
    }
  }

  test("tail of empty list throws exception") {
    new Fixture {
      assertThrows[NoSuchElementException] {
        empty.tail
      }
    }
  }

  test("Cons isEmpty returns false") {
    new Fixture {
      assert(!l.isEmpty)
    }
  }

  test("single element list head") {
    new Fixture {
      assert(l.head === 1)
    }
  }

  test("single element list tail") {
    new Fixture {
      assert(l.tail.isInstanceOf[Nil[Int]])
    }
  }

  test("empty list object") {
    new Fixture {
      assert(le.isEmpty)
      assert(le.isInstanceOf[Nil[Int]])
    }
  }

  test("single element list object") {
    new Fixture {
      assert(!l1.isEmpty)
      assert(l1.isInstanceOf[Cons[Int]])
      assert(l1.head === 42)
      assert(l1.tail.isInstanceOf[Nil[Int]])
    }
  }

  test("two elements list object") {
    new Fixture {
      assert(!l2.isEmpty)
      assert(l2.isInstanceOf[Cons[Int]])
      assert(l2.head === 12)
      assert(l2.tail.isInstanceOf[Cons[Int]])
      assert(l2.tail.head === 42)
      assert(l2.tail.tail.isInstanceOf[Nil[Int]])
    }
  }

}
