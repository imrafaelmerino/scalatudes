package etudes
import scala.collection.immutable
import scala.collection.immutable.Seq
import types.Grid
import decorators.*
import etudes.RemoveIslands.Pos
object RemoveIslands extends App:

  type Matrix = Seq[Seq[Int]]
  type Pos = (Int,Int)

  def removeIslands(matrix:Matrix):Matrix =
    def nRows = matrix.size
    def rowSize(x:Int) = matrix(x).size
    def isEdge(pos:Pos):Boolean =
      val (x,y) = pos
      y == nRows - 1 || y == 0 || x == 0 || x == rowSize(y) - 1
    
    def neighbors(pos: Pos,visited:Map[Pos,Boolean]): Seq[Pos] =
      val (row, column) = pos
      val allneighbors = Seq((row, column + 1), (row, column - 1), (row + 1, column), (row - 1, column))
      val isInMatrix: Pos => Boolean = (x, y) => x >= 0 && x < matrix.size && y >= 0 & y < matrix(row).size
      allneighbors.filter(isInMatrix).filter(!visited(_))

    def anyNeighborConnected(pos:Pos,visited:Map[Pos,Boolean]):Boolean =
      neighbors(pos,visited).find(isConnected(_, visited.updated(pos,true)))
                            .isDefined
    
    def isConnected(pos:Pos, visited: Map[Pos, Boolean]):Boolean =
      matrix(pos._1)(pos._2) == 1 && (isEdge(pos) || anyNeighborConnected(pos, visited))


    def iterMatrix(matrix:Matrix, result:Matrix=Seq.empty, nRow:Int=0):Matrix =
      def iterRow(row:Seq[Pos],acc:Seq[Int]):Seq[Int] =
        if row.isEmpty then acc
        else
          val pos = row.head
          if isConnected(pos,Map.empty.withDefaultValue(false))
          then iterRow(row.tail, acc :+ 1)
          else iterRow(row.tail, acc :+ 0)
      if matrix.isEmpty
      then result
      else
        val row:Seq[Pos] = matrix.head.zipWithIndex.map((_, index) => (nRow,index))
        iterMatrix(matrix.tail,result.appended(iterRow(row,Seq.empty)),nRow+1)


    iterMatrix(matrix)
