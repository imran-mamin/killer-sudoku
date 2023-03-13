package scala

import collection.mutable.Buffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestGame extends AnyFlatSpec with Matchers:
  // Testing Puzzleboard-class's functions
  "addNumber" should "store a number to the Tile-object" in {
    val subareaOfTwoTiles: Subarea = Subarea(2, allTiles, allTiles(0))
    val allTiles: Vector[Tile] = Vector(Tile(0, 0, 1, subareaOfTwoTiles), Tile(1, 1, 1, subareaOfTwoTiles))
    val puzzleboard: Puzzleboard = Puzzleboard(allTiles, Vector(subareaOfTwoTiles))
    puzzleboard.addNumber(0, 2)
    assert(allTiles(0).currentNumber == Some(2))

    // Tile(column: Int, row: Int, square: Int, subArea: Subarea)
    // Subarea(targetSum: Int, tiles: Vector[Tile], tileWithTargetSum: Tile)
    // Puzzleboard(allTiles: Vector[Tile], subareas: Vector[Subarea])
  }

end TestGame

