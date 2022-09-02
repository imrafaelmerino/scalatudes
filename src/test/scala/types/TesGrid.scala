package types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TesGrid extends AnyFlatSpec with should.Matchers{


  "columns and row constructors" should "return the same grid if represent the same matrix" in {

    val xs = Grid.columns(Seq(Seq(1, 2, 3), Seq(4, 5,6)))

    val ys = Grid.rows(Seq(Seq(1, 4), Seq(2, 5), Seq(3,6)))

    xs should be(ys)


  }

}
