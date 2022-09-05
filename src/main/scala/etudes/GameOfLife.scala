package etudes
import types.Grid
import types.Pos
import decorators.*
import scala.collection.immutable.Set
import scala.io.Source
object GameOfLife:
  extension (grid:Grid[String]){

    /**
     * the dessert is infinite, so before a new generation is produced, we check if
     * empty squares have to be created to accommodate the possible new born cells.
     * Only adjacent cells are taking into account, so we only need to expand at
     * the most one column ( left and/or right) and one row (top and/or botton)
     *
     * @return
     */
    def expandDesert: Grid[String] =
      val up = if grid.top.count(_._2 == CELL) > 0 then 1 else 0
      val down = if grid.bottom.count(_._2 == CELL) > 0 then 1 else 0
      val right = if grid.right.count(_._2 == CELL) > 0 then 1 else 0
      val left = if grid.left.count(_._2 == CELL) > 0 then 1 else 0
      grid.fill((down, up), (left, right), BLANK)
    def countNeighborCells(pos: Pos):Int =
      grid.neighbors(pos)
          .count(grid(_) == CELL)

    def getCells: Iterator[(Pos,String)] =
      grid.iterator.filter(_._2 == CELL)
  }


  private val BLANK = " "
  private val CELL = "\u03BF"


  def gen(grid:Grid[String]):LazyList[Grid[String]] =
    val a = deaths(grid)
    val b = births(grid)
    val new_generation = (a ++ b).expandDesert
    new_generation #:: gen(new_generation)

  //only the new births
  def births(grid:Grid[String]):Grid[String] =
    val candidates:Set[Pos] =
      grid.getCells
          .flatMap((pos,_)=> grid.neighbors(pos))
          .filter(grid(_) == BLANK)
          .toSet
    val b = candidates.filter(grid.countNeighborCells(_) == 3)
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
    val grid = Grid.parseFile2Grid("/Users/rmerino/Projects/scalatudes/src/main/chess_board.txt")

    grid.printRows(default = BLANK)
    println()

    gen(grid.expandDesert).take(10).foreach(g=> {
      g.printRows(default = BLANK,ignoreDefaultLines = true)
      println(" "*100)
    })

