package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t1) === List('a', 'b'))
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("character frequencies") {
    assert(times(List()) === List())
    assert(times(List('a', 'a', 'a')) === List(('a', 3)))
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
    assert(times(List('a', 'b', 'a', 'c', 'a', 'c')) ===
      List(('a', 3), ('b', 1), ('c', 2)))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("empty list is not a singleton") {
    assert(!singleton(List()))
  }

  test("is a singleton") {
    new TestTrees {
      assert(singleton(List(t1)))
      assert(singleton(List(t2)))
    }
  }

  test("is not a singleton") {
    new TestTrees {
      assert(!singleton(List(t1, t2)))
    }
  }

  test("combine empty list returns empty list") {
    assert(combine(List()) === List())
  }

  test("combine single list returns the same list") {
    new TestTrees {
      assert(combine(List(t1)) === List(t1))
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))

    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine until singleton") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))

    assert(until(singleton, combine)(leaflist) ===
      List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)))
  }

  test("create code tree") {
    val text = string2Chars("ettxxxx")

    assert(createCodeTree(text) ===
      Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7))
  }

  trait treeAndBites {
    val tree = createCodeTree(string2Chars("AAABBC"))
    val binA = List(1)
    val charA = List('A')
    val binC = List(0, 0)
    val charC = List('C')
    val charAbc = List('A', 'B', 'C')
    val binAbc = List(1, 0, 1, 0, 0)
    val charCba = List('C', 'B', 'A')
    val binCba = List(0, 0, 1, 0, 1)
  }

  test("decode") {
    new treeAndBites {
      assert(decode(tree, binA) === charA)
      assert(decode(tree, binC) === charC)
      assert(decode(tree, binAbc) === charAbc)
      assert(decode(tree, binCba) === charCba)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
