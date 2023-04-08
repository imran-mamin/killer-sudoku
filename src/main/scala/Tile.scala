package sudoku

import scala.collection.mutable.Buffer
import java.awt.Color

class Tile(column: Int, row: Int, square: Int):
  val candidates: Buffer[Int] = (1 to 9).toBuffer // Buffer()
  var neighbors: Buffer[Tile] = Buffer()
  var currentNumber: Option[Int] = None
  var subareaIndex: Option[Int] = None
  
  // Coordinates of the tile in the gui
  var xCoord: Double = 0
  var yCoord: Double = 0
  // These could be redundant TODO: Check variables below.
  var color: Option[Color] = None
  var targetSum: Option[Int] = None

  def getSquare: Int = square
  def getColumn: Int = column
  def getRow: Int = row

end Tile