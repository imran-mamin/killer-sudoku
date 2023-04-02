package sudoku

import scala.collection.mutable.Buffer
import java.awt.Color

class Tile(column: Int, row: Int, square: Int):
  val candidates: Buffer[Int] = Buffer()
  var neighbors: Vector[Tile] = Vector()
  var currentNumber: Option[Int] = None
  
  // These could be redundant TODO: Check variables below.
  var color: Option[Color] = None
  var targetSum: Option[Int] = None

  def getSquare: Int = square
  def getColumn: Int = column
  def getRow: Int = row
  
end Tile