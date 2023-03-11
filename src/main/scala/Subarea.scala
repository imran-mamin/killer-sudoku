package scala

import java.awt.Color
import scala.collection.mutable.Buffer

class Subarea(targetSum: Int, tiles: Vector[Tile], tileWithTargetSum: Tile):
  var neighbors: Vector[Subarea] = Vector()
  var color: Option[Color] = None
  val preSubarea: Buffer[Subarea] = Buffer()
  val postSubarea: Buffer[Subarea] = Buffer()
  
end Subarea

