package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val single_leaf = List(Leaf('a',1))
    val two_leafs = List(Leaf('a',1),Leaf('b',2))
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  
  test("times") {
    assert(times(List()) === List())
    assert(times(List('a')) === List(('a',1)))
    val res = times(List('a','b','a','d')) sortWith ((e1: (Char, Int), e2: (Char, Int)) => e1._1 < e2._1) 
    assert(res === List(('a',2),('b',1),('d',1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("singleton") {
    new TestTrees {
      assert(singleton(single_leaf))
      assert(!singleton(two_leafs))
    }
  }
  
  test("reduce list of code trees to a single tree") {
    new TestTrees {
      assert(until(singleton, combine)(single_leaf) == single_leaf)
      assert(until(singleton, combine)(two_leafs) == List(Fork(Leaf('a',1),Leaf('b',2),List('a','b'),3)))
    }
  }
  
  test("createCodeTree") {
    val text = List('c','a','a','d','c','a','c','a','a')
    val a_leaf = Leaf('a', 5)
    val c_leaf = Leaf('c', 3)
    val d_leaf = Leaf('d', 1)
    val dc_fork = Fork(d_leaf, c_leaf, List('d','c'), 4)
    val root = Fork(dc_fork, a_leaf, List('d','c','a'), 9)
    assert(createCodeTree(text) === root)
  }
  
  test("decode") {
    val code = Fork(Leaf('a', 1),Leaf('b', 1),List('a','b'),2)
    assert(decode(code, List(0)) === List('a'))
    new TestTrees {
      assert(decode(t2, List(0,0,0,1,1)) === List('a','b','d'))
    }
  }
  
  test("what's the secret?") {
    assert(decodedSecret === string2Chars("huffmanestcool"))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("convert") {
    new TestTrees {
      assert(convert(t2) === List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
    }
  }

  test("what's the secret, the quick way?") {
    assert(quickEncode(frenchCode)(decodedSecret) === secret)
  }

}
