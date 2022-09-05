package etudes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RemoveIslandsTest extends AnyFlatSpec with should.Matchers {

  "" should "" in {
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

    RemoveIslands.removeIslands(rows).sorted should be(expected)


  }

}
