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

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("created code tree for a given character list has correct weight") {
    new TestDictionaryLists {

      val codeTree = createCodeTree(listWithoutPunctuation)

      assert(weight(codeTree) == 10)

    }
  }


  ignore("encoding string should return a codeTree") {
    new TestDictionaryLists {
      val codeTree = createCodeTree(listWithPunctuation)


    }
  }

  test("codeTree conversion to table correctly builds") {
    new TestTrees {

      val testTable = convert(t1)

      assert(testTable.contains( ('a',List(0))  ))

      assert(testTable.contains( ('b',List(1))  ))
    }
  }

  test("convert on larger tree correctly builds codeTable") {
    new TestTrees {
      val testTable = convert(t2)

      assert(testTable.contains(  ('a', List(0,0))  ))

      assert(testTable.contains(  ('b', List(0,1))  ))

      assert(testTable.contains(  ('d', List(1))  ))
    }
  }

  test("codeBits correctly returns bits of given chars") {

    new TestTrees {
      val testTable = convert(t2)

      assert(codeBits(testTable)('a') == List(0,0))

      assert(codeBits(testTable)('b') == List(0 ,1))

      assert(codeBits(testTable)('d') == List(1))

    }

  }

  test("decodeIter correctly returns char list using given codeBits and tree: 1") {

    new TestTrees {
      val testCode = List(0, 1, 0, 0, 1)

      val decodedCharList = decodeIter(t1, t1, testCode)

      assert(decodedCharList == List('a', 'b', 'a', 'a', 'b') )
    }

  }

  test("decodeIter correctly returns char list using given codeBits and tree: 2") {

    new TestTrees {
      val testCode = List(0, 0, 0, 1, 1, 1, 0, 1, 0, 0)

      val decodedCharList = decodeIter(t2, t2, testCode)

      assert(decodedCharList == List('a', 'b', 'd', 'd', 'b', 'a') )
    }

  }

  test("decode correctly returns char list using given codeBits: 1") {
    new TestTrees {
      val testCode = List(0, 1, 0, 0, 1)

      val decodedCharList = decode(t1, testCode)

      assert(decodedCharList == List('a', 'b', 'a', 'a', 'b') )
    }
  }

  test("decode correctly returns char list using given codeBits and tree: 2") {

    new TestTrees {
      val testCode = List(0, 0, 0, 1, 1, 1, 0, 1, 0, 0)

      val decodedCharList = decode(t2, testCode)

      assert(decodedCharList == List('a', 'b', 'd', 'd', 'b', 'a') )
    }

  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {

      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
