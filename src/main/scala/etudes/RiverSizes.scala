package etudes

import scala.collection.immutable.Seq
import scala.collection.mutable

object RiverSizes:

  def rivers(matrix: Seq[Seq[Int]]): Seq[Int] =
    val visited = mutable.Map[(Int, Int), Boolean]().withDefaultValue(false)

    def solve(positions: Seq[(Int, Int)], rivers: Seq[Int] = Seq.empty): Seq[Int] =
      def neighbors(pos: (Int, Int)): Seq[(Int, Int)] =
        val (row, column) = pos
        val allneighbors = Seq((row, column + 1), (row, column - 1), (row + 1, column), (row - 1, column))
        val isInMatrix: ((Int, Int)) => Boolean = (i, j) => i >= 0 && i < matrix.size && j >= 0 & j < matrix(row).size
        allneighbors.filter(isInMatrix)
                    .filter(!visited(_))

      def countOnes(pos: (Int, Int)): Int =
        visited.update(pos, true)
        val (row, column) = pos
        val value = matrix(row)(column)
        if value == 0
        then 0
        else 1 + neighbors(pos).map(countOnes).sum

      if positions.isEmpty then return rivers
      val head = positions.head
      if visited(head) then return solve(positions.tail, rivers)
      val length = countOnes(head)
      solve(positions.tail, if length == 0 then rivers else rivers :+ length)

    val positions = for
      (rows, i) <- matrix.zipWithIndex
      (_, j) <- rows.zipWithIndex
    yield (i, j)

    solve(positions)


