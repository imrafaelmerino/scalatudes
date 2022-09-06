package etudes
import types.Grid
import types.Pos
import decorators.*
import scala.collection.immutable.Set
import scala.io.Source

/**
 * 
 * @param cellSymbol cells die and turn into empty symbols
 * @param emptySymbol cell are born and empty symbols turn into cell symbols
 */
class GameOfLife(val cellSymbol:String="\u03BF", val emptySymbol:String = " "):
  
  /**
   * represent an empty space that can be turned into a cell
   */
  extension (grid:Grid[String]){

    /**
     * the dessert is infinite, so before a new generation is produced, we check if
     * empty squares have to be created to accommodate the possible new born cells.
     * Only adjacent cells are taking into account, so we only need to expand at
     * the most one column ( left and/or right) and one row (top and/or botton)
     *
     * @return
     */

    def expand: Grid[String] =
      val extraTopRow= grid.topBorder.find(_._2 == cellSymbol).fold(0)(_=>1)
      val extraBottomRow = grid.bottomBorder.find(_._2 == cellSymbol).fold(0)(_=>1)
      val extraRightColumn = grid.rightBorder.find(_._2 == cellSymbol).fold(0)(_=>1)
      val extraLeftColumn = grid.leftBorder.find(_._2 == cellSymbol).fold(0)(_=>1)

      grid.fill((extraBottomRow, extraTopRow ),
                (extraLeftColumn , extraRightColumn),
                 emptySymbol
               )

    /**
     * given a position, it returns all its neighbors that contain a cell
     * @param pos the position
     * @return all the cells at adjacent positions
     */
    def countNeighborCells(pos: Pos):Int = grid.neighbors(pos).count(grid(_) == cellSymbol) /**
     *
     * @return all the cells of a grid
     */
    def getCells: Iterator[(Pos,String)] = grid.iterator.filter(_._2 == cellSymbol)
  }

  /**
   * returns a stream of generations
   * @param dessert the initial state
   * @return
   */
  def gen(dessert:Grid[String]):LazyList[Grid[String]] =
    val expanded = dessert.expand
    val removedDeaths = removeDeaths(expanded)
    val newBirths = births(expanded)
    val new_generation = removedDeaths ++ newBirths
    new_generation #:: gen(new_generation)

  def births(dessert:Grid[String]):Grid[String] =
    val candidates:Set[Pos] =
      dessert.getCells
          .flatMap((pos,_)=> dessert.neighbors(pos))
          .filter(dessert(_) == emptySymbol)
          .toSet
    val newBirths: Set[Pos] = candidates.filter(dessert.countNeighborCells(_) == 3)

    Grid(newBirths.map((_, cellSymbol)).toMap)
  

  //the generation minus the deaths
  def removeDeaths(grid:Grid[String]):Grid[String] =
    val cells: Seq[(Pos, String)] = grid.getCells.toSeq
    val deaths = cells.map(_._1).filter(pos => {
      val n = grid.countNeighborCells(pos)
      n < 2 || n > 3
    })
    grid ++ deaths.map((_,emptySymbol))


