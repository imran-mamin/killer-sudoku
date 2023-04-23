package sudoku

import scala.collection.mutable.Buffer
import java.awt.Color

/**
 * This class represent a single "small" square on the Puzzleboard. In program they will be called
 * tiles.
 * @param column tells the column of the Tile-instance on the Puzzleboard.
 * @param row tells the row of the Tile-instance on the Puzzleboard.
 * @param square tells in which square the Tile-object is placed.
 */

class Tile(column: Int, row: Int, square: Int):
  var neighbors: Buffer[Tile] = Buffer() // Will contain all the neighbors of this Tile-instance
  var currentNumber: Option[Int] = None  // Will contain a number, which is placed in the Tile. If there is no number then the value is None.
  var subareaIndex: Option[Int] = None   // Every tile knows to which sub-area it belongs to. Indexation is same as order in Puzzleboard.
  
  // Coordinates of the tile in the gui
  var xCoord: Double = 0
  var yCoord: Double = 0

  // These could be redundant TODO: Check variables below.
  var color: Option[Color] = None
  var targetSum: Option[Int] = None
  // Returns the 3x3 square, where this Tile-instance is placed.
  def getSquare: Int = square
  // Returns the column of the Tile-instance.
  def getColumn: Int = column
  // Returns the row of the Tile-instance.
  def getRow: Int = row

end Tile