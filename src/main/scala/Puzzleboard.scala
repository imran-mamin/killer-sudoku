package sudoku

import scala.collection.mutable.Buffer
import java.awt.Color

class Puzzleboard(allTiles: Vector[Tile], subareas: Vector[Subarea]):
  // var colors: Vector[Color] = Vector(Color(255, 179, 179), Color(255, 77, 77), Color(153, 0, 0), Color(255, 214, 153), Color(255, 153, 0), Color(204, 255, 204), Color(51, 255, 51), Color(0, 128, 0), Color(255, 255, 0), Color(128, 128, 0), Color(230, 230, 255), Color(153, 153, 255), Color(0, 0, 255), Color(0, 0, 102), Color(236, 179, 255), Color(210, 77, 255), Color(57, 0, 77), Color(255, 153, 230), Color(179, 255, 255), Color(26, 255, 255), Color(0, 179, 179))

  def makeGraph(): Unit = ???
  def paintGraph(): Unit = ???

  def showTiles() = allTiles
  def showSubareas() = subareas
  
  /**
   * This method sets a number to the Tile-object, which index in "allTiles"-Vector is given as a
   * parameter of the function.
   *
   * @param tileIndex index of the Tile in the "allTiles"-Vector.
   * @param num the number, which will be placed in the Tile.
   */

  def addNumber(tileIndex: Int, num: Int): Unit =
    allTiles(tileIndex).currentNumber = Some(num)

  /**
   * This method removes a number from the Tile, if there is one.
   * @param tileIndex is an index of a Tile in "allTiles"-Vector.
   */

  def removeNumber(tileIndex: Int): Unit =
    allTiles(tileIndex).currentNumber = None

  /**
   * Returns the number wrapped in an Option-data structure, which is placed in the Tile, if there is
   * one. If there is no number placed in the given Tile-object, then this method will return None.
   * @param tileIndex
   * @return If there is a number, then Some(Int), else None.
   */
  def getTileNumber(tileIndex: Int): Option[Int] =
    allTiles(tileIndex).currentNumber

  /**
   * This method tells us in which subarea given tile is placed using color.
   * @param tileIndex
   * @return If Tile-object has color, then Some(Color), else None
   */

  def getTileColor(tileIndex: Int): Option[Color] =
    allTiles(tileIndex).color

  /**
   * Returns the number wrapped in an Option that tells us, what the sum of the subarea
   * should be.
   * @param tileIndex
   * @return If Tile-object contains targetSum, then Some(Int), else None.
   */

  def getTileTargetSum(tileIndex: Int): Option[Int] =
    allTiles(tileIndex).targetSum


  def rowCandidates(index: Int): Buffer[Int] =
    val candidates = (1 to 9).toBuffer
    val row: Int = allTiles(index).getRow

    for i <- allTiles.indices do
      if (allTiles(i).getRow == row) && (allTiles(i).currentNumber.isDefined) then
        candidates -= allTiles(i).currentNumber.get
      end if
    end for
    candidates



  private def colCandidates(index: Int) =
    val candidates = (1 to 9).toBuffer
    val column: Int = allTiles(index).getColumn

    for i <- allTiles.indices do
      if (allTiles(i).getColumn == column) && (allTiles(i).currentNumber.isDefined) then
        candidates -= allTiles(i).currentNumber.get
      end if
    end for
    candidates



  private def squareCandidates(index: Int) =
    val candidates = (1 to 9).toBuffer
    val square: Int = allTiles(index).getSquare

    for i <- allTiles.indices do
      if (allTiles(i).getSquare == square) && (allTiles(i).currentNumber.isDefined) then
        candidates -= allTiles(i).currentNumber.get
      end if
    end for
    candidates


  def subareaCandidates(index: Int): Buffer[Int] =
    try
      val subareaIndex: Int = allTiles(index).subareaIndex.get
      var remaining = subareas(subareaIndex).getTargetSum()
      val tilesInSubarea: Vector[Tile] = subareas(subareaIndex).showTiles()
      var tilesWithoutNum: Int = 0
      var candidates = Buffer[Int]()


      for i <- tilesInSubarea.indices do
        if tilesInSubarea(i).currentNumber.isDefined then
          remaining -= tilesInSubarea(i).currentNumber.get
        else
          tilesWithoutNum += 1
      end for

      if tilesWithoutNum == 1 then
        candidates += remaining
      else
        if (tilesInSubarea.length == 2) && (remaining % 2 == 0) then
          val impossibleNum = remaining / 2 // For example 4 + 4 = 8, but it's not possible on sudoku board
          candidates = (1 until remaining).toBuffer
          candidates -= impossibleNum
        else
          candidates = (1 until remaining).toBuffer
      candidates
    catch
      case e => throw e


  def getCandidates(index: Int): Buffer[Int] =
    val candidatesRow   = this.rowCandidates(index)
    val candidatesCol = this.colCandidates(index)
    val candidatesSqr = this.squareCandidates(index)
    val candidatesSba = this.subareaCandidates(index)

    var intersection = candidatesRow.intersect(candidatesCol)
    intersection = intersection.intersect(candidatesSqr)
    intersection = intersection.intersect(candidatesSba)
    intersection

end Puzzleboard
