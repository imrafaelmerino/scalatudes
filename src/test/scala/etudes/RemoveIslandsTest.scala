package etudes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RemoveIslandsTest extends AnyFlatSpec with should.Matchers {

  "remove islands" should "remove block os 1 not connected to the borders" in {
    val rows = Seq(
      Seq(1, 0, 0, 0, 0, 0),
      Seq(0, 1, 0, 1, 1, 1),
      Seq(0, 0, 1, 0, 1, 0),
      Seq(1, 1, 0, 0, 1, 0),
      Seq(1, 0, 1, 1, 0, 0),
      Seq(1, 0, 0, 0, 0, 1)
    )

    val expected = Seq(
      Seq(1, 0, 0, 0, 0, 0),
      Seq(0, 0, 0, 1, 1, 1),
      Seq(0, 0, 0, 0, 1, 0),
      Seq(1, 1, 0, 0, 1, 0),
      Seq(1, 0, 0, 0, 0, 0),
      Seq(1, 0, 0, 0, 0, 1)
    )

    RemoveIslands.removeIslands(rows) should be(expected)


  }


  "remove islands II" should "remove block os 1 not connected to the borders" in {
    val rows = 
      Seq(
        Seq(1, 0, 0, 0, 0), 
        Seq(0, 1, 0, 1, 1), 
        Seq(0, 0, 1, 0, 1), 
        Seq(1, 1, 0, 0, 1), 
        Seq(1, 0, 1, 1, 0), 
        Seq(1, 0, 0, 1, 0)
      )

    val expected = Seq(
      Seq(1, 0, 0, 0, 0), 
      Seq(0, 0, 0, 1, 1), 
      Seq(0, 0, 0, 0, 1), 
      Seq(1, 1, 0, 0, 1), 
      Seq(1, 0, 1, 1, 0), 
      Seq(1, 0, 0, 1, 0)
    )

    RemoveIslands.removeIslands(rows) should be(expected)


  }


}
