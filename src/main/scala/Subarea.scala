package sudoku

import java.awt.Color
import scala.collection.mutable.Buffer

class Subarea(targetSum: Int, tiles: Vector[Tile], tileWithTargetSum: Tile):
  var neighbors: Vector[Subarea] = Vector()
  var color: Option[Color] = None

  
  def showTiles(): Vector[Tile] = tiles

  def getTargetSum(): Int = targetSum
  
  // These may be redundant TODO: Check this out later, when implementing graph-coloring
  val preSubarea: Buffer[Subarea] = Buffer()
  val postSubarea: Buffer[Subarea] = Buffer()
  
end Subarea

