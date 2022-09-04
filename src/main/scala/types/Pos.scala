package types

import java.lang.Math.pow
import scala.annotation.tailrec



case class Pos(x:Int, y:Int):
  def +(a: Pos): Pos = Pos(a.x + x, a.y + y)

  def -(a: Pos): Pos = Pos(a.x - x, a.y - y)

  def neighbors: Seq[Pos] =
    Seq(Pos(x + 1, y), Pos(x - 1, y), Pos(x, y + 1),
        Pos(x, y - 1), Pos(x + 1, y + 1), Pos(x + 1, y - 1), 
        Pos(x - 1, y + 1), Pos(x - 1, y - 1)
       )

  def right = Pos(x + 1, y)

  def left = Pos(x - 1, y)

  def up = Pos(x, y + 1)

  def down = Pos(x, y - 1)

  def manhattan_distance(pos1: Pos): Int = (x - pos1.x).abs + (y - pos1.y).abs

  def euclidean_distance(pos1: Pos): Double = Math.sqrt(pow(x - pos1.x, 2) + pow(y - pos1.y, 2))


object Pos:

  def apply(pos:(Int,Int)) = new Pos(pos._1,pos._2)
  /**
   *
   * 
   * x_range=[0,2]
   * y_range=[0,3]
   *
   * (0,0),(0,1),(0,2),(0,3),
   * (1,0),(1,1),(1,2),(1,3),
   * (2,0),(2,1),(2,2),(2,3)
   *
   * @param x_range range of x coordinates
   * @param y_range range of y coordinates
   * @return */
  def columns(x_range: Range, y_range: Range): Iterable[Pos] =
    assertRange(x_range)
    assertRange(y_range)
    val x_end = if x_range.isInclusive then x_range.end else x_range.end - 1
    val y_end = if y_range.isInclusive then y_range.end else y_range.end - 1
    @tailrec def add(x: Int, y: Int, pos: Seq[Pos] = Seq.empty): Iterable[Pos] =
      if x == x_end  && y == y_end
      then pos :+ Pos(x, y)
      else 
        if y == y_end
        then add(x + 1, y_range.start, pos :+ Pos(x, y))
        else add(x, y + 1, pos :+ Pos(x, y))

    add(x_range.start, y_range.start)

  /**
   *
   * x_range=[0,2]
   * y_range=[0,3]
   *
   * (0,0),(1,0),(2,0),
   * (0,1),(1,1),(2,1),
   * (0,2),(1,2),(2,2),
   * (0,3),(1,3),(2,3)
   * 
   * @param x_range range of x coordinates
   * @param y_range range of y coordinates
   * @return */
  def rows(x_range: Range, y_range: Range): Iterable[Pos] = 
    assertRange(x_range)
    assertRange(y_range)
    val x_end = if x_range.isInclusive then x_range.end else x_range.end - 1
    val y_end = if y_range.isInclusive then y_range.end else y_range.end - 1
    @tailrec def add(x: Int, y: Int, pos: Seq[Pos] = Seq.empty): Iterable[Pos] =
      if x == x_end && y == y_end
      then pos :+ Pos(x, y)
      else 
        if x == x_end 
        then add(x_range.start, y + 1, pos :+ Pos(x, y))
        else add(x + 1, y, pos :+ Pos(x, y))

    add(x_range.start, y_range.start)


