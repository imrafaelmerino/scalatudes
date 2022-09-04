package types

import scala.annotation.tailrec
import scala.util.Try
import types.Pos

import java.io.File
import scala.collection.immutable
import scala.collection.immutable.Set
import scala.io.Source

/**
 * represent a grid of values of type T
 * @param points map holding the grid positions and their values
 * @tparam T the type of the values
 */
case class Grid[T](points: Map[Pos,T]) extends Iterable[(Pos,T)]:

  def apply(pos:Pos):T = points(pos)
  override def iterator:Iterator[(Pos,T)] = points.iterator

  def withDefaultValue(value:T):Grid[T] = Grid[T](points.withDefaultValue(value))
  def withDefault(fn:Pos=>T):Grid[T] = Grid[T](points.withDefault(fn))

  override def equals(obj: Any): Boolean =
    obj match
      case grid:Grid[_] => grid.points == points
      case _ => false

  /**
   * four corners of the grid: (xmin, ymin), (xmin, ymax), (xmax, ymin) and (xmax, ymax) and their values
   */
  lazy val corners: Seq[(Pos,T)] =
    val a = Pos(xmin, ymin)
    val b = Pos(xmin, ymax)
    val c = Pos(xmax, ymin)
    val d = Pos(xmax, ymax)
    Seq((a,points(a)), (b,points(b)), (c,points(c)), (d,points(d)))

  /**
   * positions such that y == ymax and their values sorted by the y coordinate in ascending order
   */
  lazy val top: Seq[(Pos,T)] = points.filter(_._1.y == ymax).toSeq.sortBy(_._1.x)

  /**
   * positions such that y == ymin and their values sorted by the x coordinate in ascending order
   * */
  lazy val bottom: Seq[(Pos,T)] = points.filter(_._1.y == ymin).toSeq.sortBy(_._1.x)

  /**
   * positions such that x == xmax and their values sorted by the y coordinate in ascending order
   * */
  lazy val right: Seq[(Pos,T)] = points.filter(_._1.x == xmax).toSeq.sortBy(_._1.y)

  /**
   * positions such that x == xmin and their values sorted by the y coordinate in ascending order
   * */
  lazy val left: Seq[(Pos,T)] = points.filter(_._1.x == xmin).toSeq.sortBy(_._1.y)

  /**
   * x coordinates of al the positions of the grid
   */
  lazy val xs: Seq[Int] = points.keys.map(_.x).toSeq

  /**
   * y coordinate of al the positions of the grid
   * */
  lazy val ys: Seq[Int] = points.keys.map(_.y).toSeq

  /**
   * smallest x coordinate of the grid
   *
   * */
  lazy val xmin: Int = xs.min

  /**
   * greatest x coordinate of the grid
   *
   * */
  lazy val xmax: Int = xs.max

  /**
   * smallest y coordinate of the grid
   *
   * */
  lazy val ymin: Int = ys.min

  /**
   * greatest y coordinate of the grid
   *
   * */
  lazy val ymax: Int = ys.max

  /**
   *
   * 111         123
   * 222   ->    123
   * 333         123
   *
   *
   */
  lazy val rotate90: Grid[T] = Grid(points.map(p => (Pos(p._1.y, p._1.x), p._2)))

  /**
   *
   * 123         321
   * 123   ->    321
   * 123         321
   *
   *
   *
   */
  lazy val flipV: Grid[T] = Grid(points.map(p => (Pos(p._1.x, -p._1.y + ymax), p._2)))

  /**
   *
   * 111         333
   * 222   ->    222
   * 333         111
   *
   *
   * */
  lazy val flipH: Grid[T] = Grid(points.map(p => (Pos(-p._1.x + xmax, p._1.y), p._2)))

  def +(pos: Pos, value:T):Grid[T] = Grid(points.updated(pos,value))

  def +(x: (Pos, T)):Grid[T] = Grid(points.updated(x._1,x._2))

  def -(pos:Pos*) =
    def rec(pos:Seq[Pos],acc:Grid[T]):Grid[T] =
      if pos.isEmpty
      then acc
      else rec(pos.tail,Grid(points.removed(pos.head)))
    rec(pos,Grid.empty)

  def --(grid:Grid[T]) : Grid[T] = --(grid.points.toIndexedSeq)

  def --(xs: Iterable[(Pos, T)]): Grid[T] = ???

  def ++(grid:Grid[T]): Grid[T] = ++(grid.points.toIndexedSeq)
  def ++(xs: Iterable[(Pos, T)]): Grid[T] =
    def rec(xs: Iterable[(Pos, T)], acc: Grid[T]): Grid[T] =
      if xs.isEmpty
      then acc
      else rec(xs.tail, acc + xs.head)
    rec(xs, this)

  /**
   * neighbors of the given pos that exist in the grid
   * @param pos the position
   */
  def neighbors(pos: Pos): Seq[Pos] = pos.neighbors.filter(points.contains)

  def translate(x: Int, y: Int): Grid[T] = Grid(points.map((p, v) => (Pos(p.x + x, p.y + y), v)))


  /**
   * 1110
   * 2220   tiles(Seq((0,0), (0,1), (1,0), (1,1))  ->
   * 3330
   *
   *
   * 11101110
   * 22202220
   * 33303330
   * 11101110
   * 22202220
   * 33303330
   *
   * @param centers  centers of the grid, being (0,0) this grid
   * @param map function that return the new value given the old value of the reference tile and the new position.
   *            If not specified, the old value is inserted
   * @return
   */
  def tiles(centers: Iterable[Pos], map: (Pos, T) => T = (pos: Pos, value: T) => value): Grid[T] =
    @tailrec
    def rec(points: Iterable[Pos],acc: Grid[T]): Grid[T] =
      if points.isEmpty
      then acc
      else
        val center = points.head
        val offset_x = (xmax - xmin + 1) * center.x
        val offset_y = (ymax - ymin + 1) * center.y
        val translated = translate(offset_x, offset_y).points.map((k, v) => (k, map(center, v)))
        rec(points.tail, acc ++ translated)

    rec(centers, Grid.empty)

  def toColumns: Seq[Seq[(Pos,T)]] =
    points.groupBy(_._1.x).toSeq.sortBy(_._1).map(_._2.toSeq.sortBy(_._1.y))

  def toRows: Seq[Seq[(Pos,T)]] =
    points.groupBy(_._1.y).toSeq.sortBy(_._1).map(_._2.toSeq.sortBy(_._1.x))


  /**
   * print the grid ( a row per line in ascending y order).
   * Take into account that y > 0 is downwards on the console
   * @param separator
   * @param predicate
   */

  def printGrid(): Unit =
    for (y <- ymin to ymax) {
      for(x <- xmin to xmax){
        if points.contains(Pos(x,y))
        then print(points(Pos(x,y)))
        else print(".")
      }
      println()
    }


  /**
   *
   * @param columns (columns down, columns up)
   * @param rows    (rows to the left, rows to the right)
   * @return */
  def fill(rows: (Int, Int), columns: (Int, Int), value: T): Grid[T] =
    val outer = Grid.from_gen(xmin - columns._1 to xmax + columns._2,
                              ymin - rows._1 to ymax + rows._2)
                             (_ => Some(value))

    outer ++ this


object Grid:
  
  def empty[T] = Grid[T](Map.empty)

  /**
   * Example:
   *
   *
   * ---------      grid1
   * ------
   *
   * ---------      grid2
   * ------
   *
   * Seq(Grid,Grid)
   *
   *
   * blocks of lines separated by blank lines are parsed into a seq of grids. Blank lines are ignored
   *
   * @param path the path of the file
   * @return a list of grids
   * */
  def parseFile2Grids(path: String): Seq[Grid[String]] =
    val file = File(path)
    assert(file.exists(), s"File $file doesnt exist.")
    def parse(lines: Seq[String], grid_rows: Seq[Seq[String]], result: Seq[Grid[String]]): Seq[Grid[String]] =
      if lines.isEmpty
      then result :+ Grid.rows(grid_rows)
      else
        val head = lines.head
        if head.isBlank
        then parse(lines.tail, Seq(), result :+ Grid.rows(grid_rows))
        else parse(lines.tail, grid_rows.appended(head.split("")), result)
    parse(Source.fromFile(file).getLines.toSeq, Seq(), Seq())

  /**
   * Parse the whole file into a grid
   * @param path the path of the file
   * @return a Grid
   */
  def parseFile2Grid(path: String): Grid[String] =
    val file = File(path)
    assert(file.exists(), s"File $file doesnt exist.")
    Grid.rows(Source.fromFile(file).getLines.toSeq.map(_.split("")))

  def columns[T](columns: Seq[Seq[T]]): Grid[T] =
    val xs = 0 until columns.size
    val ys = 0 until columns.map(_.length).max
    from_gen[T](xs, ys)(pos => Try(columns(pos.x)(pos.y)).toOption)

  def rows[T](rows: Seq[Seq[T]]): Grid[T] =
    val xs = 0 until rows.map(_.length).max
    val ys = 0 until rows.size
    from_gen[T](xs,ys)(pos => Try(rows(pos.y)(pos.x)).toOption)

  def from_gen[T](x_range: Range, y_range: Range)(gen: Pos => Option[T]): Grid[T] =
    assertRange(x_range)
    assertRange(y_range)
    val x_end = if x_range.isInclusive then x_range.end else x_range.end - 1
    val y_end = if y_range.isInclusive then y_range.end else y_range.end - 1
    @tailrec
    def point_rec(x: Int, y: Int, grid: Grid[T]): Grid[T] =
      def update(pos:Pos,opt:Option[T]) = opt.fold(grid)(grid + (pos,_))
      val value = gen(Pos(x, y))
      if x == x_end && y == y_end then update(Pos(x,y),value)
      else if x == x_end then point_rec(x_range.start, y + 1, update(Pos(x,y),value))
      else point_rec(x + 1, y, update(Pos(x,y),value))

    point_rec(x_range.start, y_range.start, empty)

