package scala

import scala.collection.mutable.Buffer
import java.awt.Color

class Tile(column: Int, row: Int, square: Int, subArea: Subarea):
  val candidates: Buffer[Int] = Buffer()
  var neighbors: Vector[Tile] = Vector()
  var currentNumber: Option[Int] = None
  var color: Option[Color] = None
  var targetSum: Option[Int] = None

end Tile