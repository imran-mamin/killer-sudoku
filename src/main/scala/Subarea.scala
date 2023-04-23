package sudoku

import java.awt.Color
import scala.collection.mutable.Buffer


/**
 * This class represents a sub-area from 2 to 4 tiles as specified in the task. It takes three
 * parameters 'targetSum', 'tiles' and 'tileWithTargetSum'.
 * @param targetSum tells the sum of the sub-area. Placed numbers in the tiles should give targetSum
 *                  as their sum.
 * @param tiles tells, which Tile-objects belongs to each sub-area.
 * @param tileWithTargetSum tells, which Tile-object contains the sum of the sub-area. This is used
 *                          later in the gui to display the sum of the sub-area.
 */
class Subarea(targetSum: Int, tiles: Vector[Tile], tileWithTargetSum: Tile):

  var neighbors: Vector[Subarea] = Vector() // contains all the neighbors of Subarea instance.
  var color: Option[Color] = None // Will contain the color of each sub-area.

  // Tells which tiles belongs to the particular sub-area.
  def showTiles(): Vector[Tile] = tiles

  // Gives the targetSum of the sub-area.
  def getTargetSum(): Int = targetSum

end Subarea
