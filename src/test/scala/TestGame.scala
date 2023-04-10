import collection.mutable.Buffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sudoku.{FileReader, Puzzleboard, Subarea, Tile}
import sudoku._

class TestGame extends AnyFlatSpec with Matchers:
  "Puzzleboard addNumber-method" should "add a number to the Tile-object" in {
    val tile1 = Tile(0, 0, 1)
    val tile2 = Tile(1, 0, 1)
    val subarea = Subarea(2, Vector(tile1, tile2), tile1)
    val board = Puzzleboard(Vector(tile1, tile2), Vector(subarea))

    board.addNumber(0, 2)
    assert(tile1.currentNumber == Some(2))
  }

  "FileReader readFile-method" should "return all the lines in the given file" in {
    val file = "src/testingData/test1.txt"
    val lines = FileReader.readFile(file)

    assert(lines == Seq("#Start", "#Date", "#Subarea:", "sum: 6", "amountOfTiles: 4", "tileSum: a1",
      "tiles: a1, a2, b1, b2", "squares: 1, 1, 1, 1"))
  }

  "FileReader readFilePuzzleBoardCfg-method" should "create a new Puzzleboard object of correct length, when the given size is" +
    "not divisible by three" in {
    val file = "src/testingData/test2_readfilepuzzleboard.txt"
    val lines = FileReader.readFile(file)

    val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1
    // When the given size is not divisible by three, then program should create a board 9 x 9
    assert(board.showTiles().length == 81)
    assert(board.showSubareas().length == 1)
  }



  "FileReader initializeTiles-method" should "create the right amount of Tile-objects" in {
    val tiles = initializeTiles(9, 9)
    assert(tiles.length == 81)
    assert(tiles.head.getSquare == 0)
    assert(tiles.last.getSquare == 8)
    assert(tiles.head.getRow == 0 && tiles.head.getColumn == 0)
    assert(tiles.last.getRow == 8 && tiles.last.getColumn == 8)
 }

  "FileReader initializeTile-method" should "initialize tile's neighbors properly" in {
    val tiles = initializeTiles(9, 9)
    assert(tiles.head.neighbors == Buffer(tiles(1), tiles(9)))
    assert(tiles(1).neighbors == Buffer(tiles(0), tiles(2), tiles(10)))
    assert(tiles(9).neighbors == Buffer(tiles(0), tiles(10), tiles(18)))
    assert(tiles(10).neighbors == Buffer(tiles(1), tiles(9), tiles(11), tiles(19)))
    assert(tiles(80).neighbors == Buffer(tiles(71), tiles(79)))
    assert(tiles(19).neighbors == Buffer(tiles(10), tiles(18), tiles(20), tiles(28)))
  }

  "FileReader readFilePuzzleboard-method" should "create a correct Puzzleboard-object" in {
    val file = "src/testingData/test3_readfilepuzzleboard.txt"
    val lines = FileReader.readFile(file)

   // Create a sudoku board of size (3 x 3)
    val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1

    // Amount of tiles should be as specified in the file
    assert(board.showTiles().length == 9)

    // All tiles should have correct row and column number.
    assert(board.showTiles().head.getRow == 0 && board.showTiles().head.getColumn == 0)
    assert(board.showTiles()(1).getRow == 0 && board.showTiles()(1).getColumn == 1)
    assert(board.showTiles()(2).getRow == 0 && board.showTiles()(2).getColumn == 2)
    assert(board.showTiles()(3).getRow == 1 && board.showTiles()(3).getColumn == 0)
    assert(board.showTiles()(4).getRow == 1 && board.showTiles()(4).getColumn == 1)
    assert(board.showTiles()(5).getRow == 1 && board.showTiles()(5).getColumn == 2)
    assert(board.showTiles()(6).getRow == 2 && board.showTiles()(6).getColumn == 0)
    assert(board.showTiles()(7).getRow == 2 && board.showTiles()(7).getColumn == 1)
    assert(board.showTiles()(8).getRow == 2 && board.showTiles()(8).getColumn == 2)

    // All tiles should be in the same square
    assert(board.showTiles().forall( tile => tile.getSquare == 0 ))
  }

  "updateCandidatesInRow()-method" should "update the candidates of tiles that are in the" +
    "same row with the tile, where the number is placed." in {
    // Puzzleboard(allTiles: Vector[Tile], subareas: Vector[Subarea])
    // Tile(column: Int, row: Int, square: Int)
    // Subarea(targetSum: Int, tiles: Vector[Tile], tileWithTargetSum: Tile)
    val tilesRow0 = Vector[Tile](new Tile(0, 0, 1), new Tile(1, 0, 1), new Tile(2, 0, 1))
    val tilesRow1 = Vector[Tile](new Tile(0, 1, 1), new Tile(1, 1, 1), new Tile(2, 1, 1))
    val tilesRow2 = Vector[Tile](new Tile(0, 2, 1), new Tile(1, 2, 1), new Tile(2, 2, 1))
    val subarea0 = new Subarea(6, tilesRow0, tilesRow0(0))
    val subarea1 = new Subarea(8, tilesRow1, tilesRow1(2))
    val subarea2 = new Subarea(9, tilesRow2, tilesRow2(1))
    val board = new Puzzleboard(tilesRow0 ++ tilesRow1 ++ tilesRow2, Vector(subarea0, subarea1, subarea2))

    // First row
    board.addNumber(2, 2)
    board.updateCandidatesInRow(2)
    val remainingCandidates0 = (1 to 9).toBuffer
    remainingCandidates0 -= 2
    assert(tilesRow0(0).candidates == remainingCandidates0)
    assert(tilesRow0(1).candidates == remainingCandidates0)

    // Second row
    board.addNumber(4, 6)
    board.updateCandidatesInRow(4)
    val remainingCandidates1 = (1 to 9).toBuffer
    remainingCandidates1 -= 6
    assert(tilesRow1(0).candidates == remainingCandidates1)
    assert(tilesRow1(2).candidates == remainingCandidates1)

    // Third row
    board.addNumber(6, 8)
    board.updateCandidatesInRow(6)
    val remainingCandidates2 = (1 to 9).toBuffer
    remainingCandidates2 -= 8
    assert(tilesRow2(1).candidates == remainingCandidates2)
    assert(tilesRow2(2).candidates == remainingCandidates2)

  }


end TestGame

