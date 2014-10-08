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
  }

  trait TestDictionaryLists {
    val listWithPunctuation = List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')
    val listWithoutPunctuation = List('h', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd')
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

  test("frequency of elements in list using times with no punctuation is correct length") {
    new TestDictionaryLists {

      assert(times(listWithoutPunctuation).length === 7)

    }
  }

  test("frequency of elements in list using times with punctuation is correct length") {

    new TestDictionaryLists {

      assert(times(listWithPunctuation).length === 9)

    }

  }

  test("frequency of elements in list using times correctly counts elements") {

    new TestDictionaryLists {

      assert(times(listWithoutPunctuation).exists((element:(Char, Int))=> element._1 == 'h' && element._2 == 1  ))

      assert(times(listWithoutPunctuation).exists((element:(Char, Int))=> element._1 == 'e' && element._2 == 1  ))

      assert(times(listWithoutPunctuation).exists((element:(Char, Int))=> element._1 == 'l' && element._2 == 3  ))

      assert(times(listWithoutPunctuation).exists((element:(Char, Int))=> element._1 == 'o' && element._2 == 2  ))

      assert(times(listWithoutPunctuation).exists((element:(Char, Int))=> element._1 == 'w' && element._2 == 1  ))

      assert(times(listWithoutPunctuation).exists((element:(Char, Int))=> element._1 == 'r' && element._2 == 1  ))

      assert(times(listWithoutPunctuation).exists((element:(Char, Int))=> element._1 == 'd' && element._2 == 1  ))

    }

  }

  test("frequency of elements in list using times correctly counts punctuation elements") {

    new TestDictionaryLists {

      assert(times(listWithPunctuation).exists((element:(Char, Int))=> element._1 == ',' && element._2 == 1  ))

      assert(times(listWithPunctuation).exists((element:(Char, Int))=> element._1 == ' ' && element._2 == 1  ))

    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  ignore("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  ignore("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
