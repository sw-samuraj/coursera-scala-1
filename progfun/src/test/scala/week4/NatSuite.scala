package week4

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NatSuite extends FunSuite {

  trait Fixture {
    val one = new Succ(Zero)
    val two = new Succ(one)
  }

  test("zero isZero is true") {
    assert(Zero.isZero)
  }

  test("zero predecessor throws an error") {
    assertThrows[Error] {
      Zero.predecessor
    }
  }

  test("successor of zero is one") {
    new Fixture {
      assert(Zero.successor == one)
    }
  }

  test("zero plus zero is zero") {
    assert((Zero + Zero) === Zero)
  }

  test("zero plus one is one") {
    new Fixture {
      assert((Zero + one) === one)
    }
  }

  test("zero minus zero is zero") {
    assert((Zero - Zero) === Zero)
  }

  test("zero minus one throws an error") {
    new Fixture {
      assertThrows[Error] {
        Zero - one
      }
    }
  }

  test("one isZero is false") {
    new Fixture {
      assert(!one.isZero)
    }
  }

  test("predecessor of one is zero") {
    new Fixture {
      assert(one.predecessor === Zero)
    }
  }

  test("successor of one is two") {
    new Fixture {
      assert(one.successor == two)
    }
  }

  test("one plus zero is one") {
    new Fixture {
      assert((one + Zero) === one)
    }
  }

  test("one plus one is two") {
    new Fixture {
      assert((one + one) === two)
    }
  }

  test("one minus zero is one") {
    new Fixture {
      assert((one - Zero) === one)
    }
  }

  test("one minus one is zero") {
    new Fixture {
      assert((one - one) === Zero)
    }
  }

  test("two minus one is one") {
    new Fixture {
      assert((two - one) === one)
    }
  }

}
