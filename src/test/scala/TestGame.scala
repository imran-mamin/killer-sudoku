import collection.mutable.Buffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sudoku.{FileReader, Puzzleboard, Subarea, Tile}
import sudoku._

class TestGame extends AnyFlatSpec with Matchers:

  "FileReader readFile-method" should "return all the lines in the given file" in {
    val file = "src/testingData/test1.txt"
    val lines = FileReader.readFile(file)

    assert(lines == Seq("#Start", "#Date", "#Subarea:", "sum: 6", "amountOfTiles: 4", "tileSum: a1",
      "tiles: a1, a2, b1, b2", "squares: 1, 1, 1, 1"))
  }
/*
  "FileReader readFilePuzzleBoardCfg-method" should "create a new Puzzleboard object of correct length, when the given size is" +
    "not divisible by three" in {
    val file = "src/testingData/test2_readfilepuzzleboard.txt"
    val lines = FileReader.readFile(file)

    val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1
    // When the given size is not divisible by three, then program should create a board 9 x 9
    assert(board.showTiles().length == 81)
    assert(board.showSubareas().length == 1)
  }
*/


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

  "rowCandidates()-method" should "return all possible candidates so that" +
    "there will be no two same digits in the same row" in {
    val row0 = Vector(new Tile(0, 0, 1), new Tile(1, 0, 1), new Tile(2, 0, 1))
    val row1 = Vector(new Tile(0, 1, 1), new Tile(1, 1, 1), new Tile(2, 1, 1))
    val row2 = Vector(new Tile(0, 2, 1), new Tile(1, 2, 1), new Tile(2, 2, 1))
    // Subarea(targetSum: Int, tiles: Vector[Tile], tileWithTargetSum: Tile)
    val subarea0 = new Subarea(6, row0, row0(0))
    val subarea1 = new Subarea(8, row1, row1(1))
    val subarea2 = new Subarea(11, row2, row2(2))

    val board = new Puzzleboard(row0 ++ row1 ++ row2, Vector(subarea0, subarea1, subarea2))

    row0(0).currentNumber = Some(1)
    assert(board.getCandidates(1) == (2 to 9).toBuffer)
    row0(1).currentNumber = Some(2)
    assert(board.getCandidates(2) == (3 to 9).toBuffer)
 
  }
/*
  "colCandidates()-method" should "return all possible candidates so that" +
    "there will be no two same digits in the same column." in {
    
  }
  */

  "twoTilesPossibleCombinations()-method" should "return possible combinations of tiles' candidates." in {
    val tile1 = Tile(0, 0, 1)
    val tile2 = Tile(1, 0, 1)
    val targetSum: Int = 4
    val subarea = Subarea(targetSum, Vector(tile1, tile2), tile1)
    val board = Puzzleboard(Vector(tile1, tile2), Vector(subarea))

    val combinations: Buffer[Vector[Int]] = board.twoTilesPossibleCombinations(1, 0, targetSum)
    assert(combinations.size == 1, "Method should return only one combination.")
    assert(combinations.contains(Vector(1, 3)), "The only combination should be '1 + 3'.")
  }

  "twoTilesPossibleCombinations()-method" should "return proper possible combinations of tiles' candidates," +
    "when the sub-area consists of three tiles and one number is already placed in one tile." in {
    val tile1 = Tile(3, 0, 2)
    val tile2 = Tile(3, 1, 2)
    val tile3 = Tile(3, 2, 2)
    var targetSum: Int = 18
    val subarea = Subarea(targetSum, Vector(tile1, tile2, tile3), tile2)
    val board = Puzzleboard(Vector(tile1, tile2, tile3), Vector(subarea))

    tile1.currentNumber = Some(4)
    targetSum -= 4
    val combinations: Buffer[Vector[Int]] = board.twoTilesPossibleCombinations(1, 0, targetSum)
    assert(combinations.size == 2, "Method should return two combinations.")
    assert(combinations.contains(Vector(5, 9)) && combinations.contains(Vector(6, 8)), s"${combinations} did not equal Buffer('5 + 9', '6 + 8')")
  }

  "twoTilesPossibleCombinations()-method" should "return proper possible combinations of tiles' candidates," +
    "when the sub-area consists of fours tiles and numbers are already placed in two tiles." in {
    val tile1 = Tile(3, 0, 2)
    val tile2 = Tile(3, 1, 2)
    val tile3 = Tile(4, 0, 2)
    val tile4 = Tile(4, 1, 2)
    var targetSum: Int = 24
    val subarea = Subarea(targetSum, Vector(tile1, tile2, tile3, tile4), tile2)
    val board = Puzzleboard(Vector(tile1, tile2, tile3, tile4), Vector(subarea))

    tile2.currentNumber = Some(6)
    tile4.currentNumber = Some(8)
    targetSum -= 6
    targetSum -= 8
    val combinations: Buffer[Vector[Int]] = board.twoTilesPossibleCombinations(3, 0, targetSum)

    // 1 + 9, 2 + 8, 3 + 7, 4 + 6 (because 8 and 6 are placed, combinations 2 + 8 and 4 + 6 should be ignored.)
    assert(combinations.size == 2, "Method should return four possible combinations.")
    assert(combinations.contains(Vector(1, 9)), s"combinations should contain '1 + 9'.")
    assert(combinations.contains(Vector(3, 7)), s"combinations should contain '3 + 7'.")

  }


  "threeTilesPossibleCombinations()-method" should "return possible combinations of tiles' candidates" in {
    val tile1 = Tile(0, 0, 1)
    val tile2 = Tile(1, 0, 1)
    val tile3 = Tile(0, 1, 1)
    val targetSum = 8
    val subarea = Subarea(targetSum, Vector(tile1, tile2, tile3), tile1)
    val board = Puzzleboard(Vector(tile1, tile2, tile3), Vector(subarea))

    val combinations: Buffer[Vector[Int]] = board.threeTilesPossibleCombinations(1, 0, targetSum)
    assert(combinations.contains(Vector(1, 2, 5)) && combinations.contains(Vector(1, 3, 4)), s"${combinations} did not equal Buffer('1 + 2 + 5', '1 + 3 + 4').")
    assert(combinations.size == 2, "The amount of combinations is not the same.")
  }


  "FileReader readFilePuzzleBoardCfg-method" should "create a new Puzzleboard object with correctly placed" +
  "nums in some tiles" in {
  val file = "src/testingData/9x9_example_board.txt"
  val lines = FileReader.readFile(file)

  val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1
  // When the given size is not divisible by three, then program should create a board 9 x 9
  assert(board.showTiles()(0).currentNumber.isDefined)
  assert(board.showTiles()(1).currentNumber.isDefined)
  }
/*
  "FileReader readFilePuzzleBoardCfg-method" should "read a Puzzleboard object properly" in {
  val file = "C:\\Users\\imran\\IdeaProjects\\Killer_Sudoku\\src\\testingData\\myfile_1.txt"
  val lines = FileReader.readFile(file)

  val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1
  // When the given size is not divisible by three, then program should create a board 9 x 9
  assert(board.showTiles()(0).currentNumber.isDefined)
  assert(board.showTiles()(1).currentNumber.isDefined)
  assert(board.showTiles()(32).currentNumber.isDefined)
  }
*/

  "addNeighborsToSubareas-method" should "add neighbors properly to sub-areas" in {
    val file = "src/testingData/9x9_example_board.txt"
    val lines = FileReader.readFile(file)

    val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1
    val allSba = board.showSubareas()
    FileReader.addNeighborsToSubareas(board)
    assert(allSba(0).neighbors.contains(allSba(1)), "First sub-area has two neighbors.")
    assert(allSba(0).neighbors.contains(allSba(6)))
    assert(allSba(0).neighbors.length == 2)

    assert(allSba(1).neighbors.contains(allSba(0)), "Second sub-area has three neighbors.")
    assert(allSba(1).neighbors.contains(allSba(2)))
    assert(allSba(1).neighbors.contains(allSba(7)))
    assert(allSba(1).neighbors.length == 3)

    assert(allSba(7).neighbors.contains(allSba(1)), "Seventh sub-area has four neighbors.")
    assert(allSba(7).neighbors.contains(allSba(2)))
    assert(allSba(7).neighbors.contains(allSba(6)))
    assert(allSba(7).neighbors.contains(allSba(8)))
    assert(allSba(7).neighbors.length == 4)
  }

  "Neigboring sub-areas" should "have different color" in {
    val file = "src/testingData/9x9_example_board.txt"
    val lines = FileReader.readFile(file)

    val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1
    val allSba = board.showSubareas()
    var differentColorInNeighbors: Boolean = true

    for i <- allSba.indices do
      val neighbors = allSba(i).neighbors
      if neighbors.exists( neighbor => neighbor.color.get == allSba(i).color.get ) then
        differentColorInNeighbors = false
      end if
    end for
    assert(differentColorInNeighbors, "Neighboring sub-areas should have different color.")
  }

end TestGame

