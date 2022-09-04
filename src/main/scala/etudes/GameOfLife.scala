package etudes
import types.Grid
import types.Pos
import decorators.*
import scala.collection.immutable.Set
import scala.io.Source
object GameOfLife:
  extension (grid:Grid[String]){

    def countNeighborCells(pos: Pos):Int =
      grid.neighbors(pos)
          .count(grid(_) == CELL)

    def getCells: Iterator[(Pos,String)] =
      grid.iterator.filter(_._2 == CELL)
  }


  private val BLANK = "."
  private val CELL = "\u03BF"

  def gen(grid:Grid[String]):LazyList[Grid[String]] =
    val a = deaths(grid)
    val b = births(grid)
    val new_generation = (a ++ b).fill((1,1),(1,1),BLANK)
    new_generation #:: gen(new_generation)

  //only the new births
  def births(grid:Grid[String]):Grid[String] =
    val candidates:Set[Pos] =
      grid.getCells
          .flatMap((pos,_)=> grid.neighbors(pos))
          .filter(grid(_) == BLANK)
          .toSet
    val b = candidates.filter(grid.countNeighborCells(_) == 3)
    println(b)
    if b.isEmpty
    then Grid.empty
    else Grid(b.map((_, CELL)).toMap)



  //the generation minus the deaths
  def deaths(grid:Grid[String]):Grid[String] =
    val cells: Seq[(Pos, String)] = grid.getCells.toSeq
    val d = cells.map(_._1).filter(pos => {
      val n = grid.countNeighborCells(pos)
      n < 2 || n > 3
    })


    grid ++ d.map((_,BLANK))




  @main
  def main:Unit =
    val grid = Grid.parseFile2Grid("/Users/rmerino/Projects/scalatudes/src/main/input.txt")

    grid.printGrid()
    println()

    gen(grid.fill((1,1),(1,1),BLANK)).take(10).foreach(g=> {
      g.printGrid()
      Thread.sleep(2000)
    })

