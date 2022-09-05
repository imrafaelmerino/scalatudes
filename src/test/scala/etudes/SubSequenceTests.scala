package etudes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SubSequenceTests extends AnyFlatSpec with should.Matchers {

  "if it's subsequence" should "return true" in {
    val seq = List(5, 1, 22, 25, 6, -1, 8, 10)
    Subsequence.isSubsequence(seq, List(1, 6, -1, 10)) should be(true)
    Subsequence.isSubsequence(seq, List(8, 10)) should be(true)
    Subsequence.isSubsequence(seq, List(5, 10)) should be(true)
    Subsequence.isSubsequence(seq, List(25, 6)) should be(true)
  }

  "if it's not subsequence" should "return false" in {
    val seq = List(5, 1, 22, 25, 6, -1, 8, 10)
    Subsequence.isSubsequence(seq, List(3, 6, -1, 10)) should be(false)

  }
}
