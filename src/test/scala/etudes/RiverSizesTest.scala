package etudes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RiverSizesTest extends AnyFlatSpec with should.Matchers {

  "I river sizes" should "return the sizes" in {

    val matrix = Seq(
      Seq(1, 0, 0, 1, 0),
      Seq(1, 0, 1, 0, 0),
      Seq(0, 0, 1, 0, 1),
      Seq(1, 0, 1, 0, 1),
      Seq(1, 0, 1, 1, 0)
    )

    RiverSizes.rivers(matrix).sorted should be(List(1,2,2,2,5))
  }

  "II river sizes" should "return the sizes" in {
    val matrix = Seq(
      Seq(1, 0, 0, 1, 0),
      Seq(1, 0, 1, 0, 0),
      Seq(0, 0, 1, 0, 1),
      Seq(1, 0, 1, 1, 1),
      Seq(1, 0, 0, 1, 0),
      Seq(0, 1, 0, 1, 0)
    )

    RiverSizes.rivers(matrix).sorted should be(List(1, 1,2,  2, 8))
  }

}
