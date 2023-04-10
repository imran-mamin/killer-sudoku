package sudoku

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


  /**
   * This method will update all tiles' candidates that are in the same row with
   * the tile, where the number is placed.
   * @param index, takes as a parameter an index of a tile, where the number is placed.
   */

  def updateCandidatesInRow(index: Int): Unit =
    try
      val row: Int = allTiles(index).getRow
      val placedNum: Int = allTiles(index).currentNumber.get

      for i <- allTiles.indices do
        val currentTile = allTiles(i)

        if currentTile.getRow == row then
          currentTile.candidates -= placedNum
        end if
      end for

    catch
      case e => throw e


  /**
   * This method will update all tiles' candidates that are in the
   * same column with the tile, where the tile is placed.
   * @param index, takes as a parameter an index of a tile, where the number is
   * placed.
   */
  def updateCandidatesInCol(index: Int): Unit =
    try
      val col: Int = allTiles(index).getColumn
      val placedNum: Int = allTiles(index).currentNumber.get

      for i <- allTiles.indices do
        val currentTile = allTiles(i)

        if currentTile.getColumn == col then
          currentTile.candidates -= placedNum
        end if
      end for
    catch
      case e => throw e

  /**
   * This method will update all tiles' candidates that are in the
   * same square with the tile, where the number is placed.
   * @param index, takes as a parameter index of a tile, where the number
   * is placed.
   */
  def updateCandidatesInSquare(index: Int): Unit =
    try
      val placedNum: Int = allTiles(index).currentNumber.get
      val square: Int = allTiles(index).getSquare
      
      for i <- allTiles.indices do
        val currentTile = allTiles(i)
        
        if currentTile.getSquare == square then
          currentTile.candidates -= placedNum
        end if
      end for
    catch
      case e => throw e
      
      
end Puzzleboard
