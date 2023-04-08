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

end TestGame

