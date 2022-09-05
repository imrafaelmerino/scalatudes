package etudes

import etudes.GameOfLife
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import types.Grid

class GameOfLifeTests extends AnyFlatSpec with should.Matchers{


  "οοοοο οοοοο οοοοο" should "generate a stable community" in {
    val dessert = Grid.parseFile("/Users/rmerino/Projects/scalatudes/src/main/data/game_of_life/etudes_for_programmers.txt")
    val gameOfLife = GameOfLife()
    println(s"generation 1:")
    println()
    dessert.printRows(default = gameOfLife.emptySymbol)
    println()
    gameOfLife.gen(dessert).take(50).zipWithIndex.foreach((g,index) => {
      println(s"generation ${index+2}")
      println()
      g.printRows(default = gameOfLife.emptySymbol)
      println()
    })

  }
  


}
