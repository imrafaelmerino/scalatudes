package etudes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import etudes.ShiftingLinkedList.LinkedList
class ShiftingLinkedListTests extends AnyFlatSpec with should.Matchers {

  "shifting a linked list" should "move elements" in {
    
    ShiftingLinkedList.shift(LinkedList.from(0,1,2,3,4,5),1) should be(LinkedList.from(5,0,1,2,3,4))

    ShiftingLinkedList.shift(LinkedList.from(0,1,2,3,4,5),2) should be(LinkedList.from(4,5,0,1,2,3))

    ShiftingLinkedList.shift(LinkedList.from(0,1,2,3,4,5),-1) should be(LinkedList.from(1,2,3,4,5,0))

  }

}
