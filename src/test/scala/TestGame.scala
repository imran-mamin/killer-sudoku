import collection.mutable.Buffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sudoku.{FileReader, Puzzleboard, Subarea, Tile}
import sudoku._
import java.awt.Color


class TestGame extends AnyFlatSpec with Matchers:

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
    assert(tiles(8).neighbors.contains(tiles(7)) && tiles(8).neighbors.contains(tiles(17)))
    assert(tiles(9).neighbors == Buffer(tiles(0), tiles(10), tiles(18)))
    assert(tiles(10).neighbors == Buffer(tiles(1), tiles(9), tiles(11), tiles(19)))
    assert(tiles(80).neighbors == Buffer(tiles(71), tiles(79)))
    assert(tiles(17).neighbors.contains(tiles(8)) && tiles(17).neighbors.contains(tiles(26)))
    assert(tiles(19).neighbors == Buffer(tiles(10), tiles(18), tiles(20), tiles(28)))
  }

  "FileReader createSubareaAndUpdateTiles" should "place the correct subareaIndex to every tile" in {
    val file = "src/testingData/9x9_example_board_2.txt"
    val lines = FileReader.readFile(file)

   // Create a sudoku board of size (3 x 3)
    val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1
    val tiles = board.getTiles
    val subareas = board.getSubareas

    assert(subareas.head.getTiles.contains(tiles(9)))
    assert(subareas.head.getTiles.contains(tiles.head))
    assert(subareas.head.getTiles.contains(tiles(1)))
    assert(tiles(9).subareaIndex.get == tiles.head.subareaIndex.get, "First and 9th tile should be in the same sub-area.")
    assert(tiles(9).subareaIndex.get == tiles(1).subareaIndex.get, "Second tile and 9th tile should be in the same sub-area.")
  }


  "FileReader addNeighborsToSubareas" should "add proper neighbors to every sub-area" in {
    val file = "src/testingData/9x9_example_board_2.txt"
    val lines = FileReader.readFile(file)

   // Create a sudoku board of size (3 x 3)
    val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1
    val allSba = board.getSubareas
    var differentColorInNeighbors: Boolean = true

    for i <- allSba.indices do
      val neighbors = allSba(i).neighbors
      if neighbors.exists( neighbor => neighbor.color.get == allSba(i).color.get ) then
        differentColorInNeighbors = false
      end if
    end for
    assert(differentColorInNeighbors, "Neighboring sub-areas should have different color.")

  }

  "FileReader readFilePuzzleboard-method" should "create a correct Puzzleboard-object" in {
    val file = "src/testingData/test3_readfilepuzzleboard.txt"
    val lines = FileReader.readFile(file)

   // Create a sudoku board of size (3 x 3)
    val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1

    // Amount of tiles should be as specified in the file
    assert(board.getTiles.length == 9)

    // All tiles should have correct row and column number.
    assert(board.getTiles.head.getRow == 0 && board.getTiles.head.getColumn == 0)
    assert(board.getTiles(1).getRow == 0 && board.getTiles(1).getColumn == 1)
    assert(board.getTiles(2).getRow == 0 && board.getTiles(2).getColumn == 2)
    assert(board.getTiles(3).getRow == 1 && board.getTiles(3).getColumn == 0)
    assert(board.getTiles(4).getRow == 1 && board.getTiles(4).getColumn == 1)
    assert(board.getTiles(5).getRow == 1 && board.getTiles(5).getColumn == 2)
    assert(board.getTiles(6).getRow == 2 && board.getTiles(6).getColumn == 0)
    assert(board.getTiles(7).getRow == 2 && board.getTiles(7).getColumn == 1)
    assert(board.getTiles(8).getRow == 2 && board.getTiles(8).getColumn == 2)

    // All tiles should be in the same square
    assert(board.getTiles.forall( tile => tile.getSquare == 0 ))
  }


  "FileReader readFilePuzzleBoardCfg-method" should "create a new Puzzleboard object with correctly placed " +
  "nums in some tiles" in {
  val file = "src/testingData/9x9_example_board.txt"
  val lines = FileReader.readFile(file)

  val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1
  // When the given size is not divisible by three, then program should create a board 9 x 9
  assert(board.getTiles(0).currentNumber.isDefined)
  assert(board.getTiles(1).currentNumber.isDefined)
  }


  "addNeighborsToSubareas-method" should "add neighbors properly to sub-areas" in {
    val file = "src/testingData/9x9_example_board.txt"
    val lines = FileReader.readFile(file)

    val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1
    val allSba = board.getSubareas
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


  // Test corrupted files and error handling
  "Assertion in FileReader" should "fail, when it reads a file without a title" in {
    val message = "assertion failed: File does not have a title." // Message that should be thrown
    val file = "src/testingData/file_without_title.txt"
    val lines = FileReader.readFile(file)

    // Should throw an assertion
    intercept[AssertionError]{
      FileReader.readFilePuzzleBoardCfg(lines)
    }.getMessage shouldEqual message
  }


  "Assertion in FileReader" should "fail, when it reads a file, which doesn't contain all information " +
    "about puzzleboard" in {
    // There is only one sub-area in this file, but rowsize and colsize are given as 9 tiles.
    val message = "assertion failed: Sub-area from top: 1, Tile column: i and row: 9 info is missing in " +
      "config file."
    val file = "src/testingData/corrupted_file_without_all_info.txt"
    val lines = FileReader.readFile(file)
    // Should throw an assertion
    intercept[AssertionError]{
      FileReader.readFilePuzzleBoardCfg(lines)
    }.getMessage shouldEqual message
  }


  "Assertion in FileReader" should "fail, when it reads a file, which has a string instead of num " +
    "at some place in the file." in {
    // One tile code is 'bl' and should have num instead of 'l' character.
    val message = "assertion failed: Sub-area from top: 25, error in the code of the tile in " +
      "tiles-keyword: 'bl'."
    val file = "src/testingData/corrupted_file_with_string_instead_of_num.txt"
    val lines = FileReader.readFile(file)
    // Should throw an assertion
    intercept[AssertionError]{
      FileReader.readFilePuzzleBoardCfg(lines)
    }.getMessage shouldEqual message
  }


  "Assertion in FileReader" should "fail, when it reads a file, which has a duplicate in sub-areas " +
    "sum property" in {
    val message = "assertion failed: Duplicate found: Sub-area from top: 29," +
      " 'sum:17'"
    val file = "src/testingData/corrupted_file_with_duplicate_sum_property.txt"
    val lines = FileReader.readFile(file)
    // Should throw an assertion
    intercept[AssertionError]{
      FileReader.readFilePuzzleBoardCfg(lines)
    }.getMessage shouldEqual message
  }


  "Assertion in FileReader" should "fail, when it reads a file, which has a duplicate in sub-areas" in {
    val message = "assertion failed: Sub-area from top: 30, Tile column: h, row: 9, duplicate found."
    val file = "src/testingData/corrupted_file_with_duplicate_subarea.txt"
    val lines = FileReader.readFile(file)
    // Should throw an assertion
    intercept[AssertionError]{
      FileReader.readFilePuzzleBoardCfg(lines)
    }.getMessage shouldEqual message
  }


  "Assertion in FileReader" should "fail, when it reads a file, which doesn't contain any " +
    "information after keyword and colon" in {
    val message = "assertion failed: Sub-area from top: 1, Sum of the sub-area is not given."
    val file = "src/testingData/corrupted_file_where_info_no_given_after_colon.txt"
    val lines = FileReader.readFile(file)
    // Should throw an assertion
    intercept[AssertionError]{
      FileReader.readFilePuzzleBoardCfg(lines)
    }.getMessage shouldEqual message
  }


  "Assertion in FileReader" should "fail, when it reads a file, where tiles are not separated by" +
    "comma." in {
    val message = "assertion failed: Sub-area from top: 29, in key tiles some tiles are not " +
      "comma separated or contain extra characters."
    val file = "src/testingData/corrupted_file_where_elements_are_not_separated_with_comma.txt"
    val lines = FileReader.readFile(file)
    // Should throw an assertion
    intercept[AssertionError]{
      FileReader.readFilePuzzleBoardCfg(lines)
    }.getMessage shouldEqual message
  }


  "Assertion in FileReader" should "fail, when it reads a file, when there is no colon " +
    "after keyword." in {
    val message = "assertion failed: Sub-area from top: 2, The line doesn't contain colon: 'sum15'"
    val file = "src/testingData/corrupted_file_where_colon_not_given.txt"
    val lines = FileReader.readFile(file)
    // Should throw an assertion
    intercept[AssertionError]{
      FileReader.readFilePuzzleBoardCfg(lines)
    }.getMessage shouldEqual message
  }

  // Test Puzzleboard methods

  "getCandidatesAfterSbaFilter-method" should "return the proper candidates that can be placed " +
    "into the given tile, when board's size is 3x3 tiles" in {
    val file = "src/testingData/test3_readfilepuzzleboard.txt"
    val lines = FileReader.readFile(file)
    val board = FileReader.readFilePuzzleBoardCfg(lines)._1
    val tiles = board.getTiles

    // Tiles are calculated using method from top to down and from left to right.
    assert(board.getCandidatesAfterSbaFilter(0).isEmpty, "First tile doesn't have any candidates.")
    assert(board.getCandidatesAfterSbaFilter(1).isEmpty, "Second tile doesn't have any candidates.")
    assert(board.getCandidatesAfterSbaFilter(2).containsSlice(Seq[Int](1, 3)), "Third tile has two candidates.")
    assert(board.getCandidatesAfterSbaFilter(3).isEmpty, "Fourth tile doesn't have any candidates.")
    assert(board.getCandidatesAfterSbaFilter(4).isEmpty, "Fifth tile doesn't have any candidates.")
    assert(board.getCandidatesAfterSbaFilter(5).containsSlice(Seq[Int](1, 3)), "Fifth tile has same candidates as third one.")
    assert(board.getCandidatesAfterSbaFilter(6).containsSlice((1 to 8).toSeq), "Sixth tile has 8 candidates.")
  }


  "getCandidatesAfterSbaFilter-method" should "return the proper candidates that can be placed " +
    "into the given tile, when board's size is 3x3 tiles and some of the numbers are placed" in {
    val file = "src/testingData/test3_readfilepuzzleboard.txt"
    val lines = FileReader.readFile(file)
    val board = FileReader.readFilePuzzleBoardCfg(lines)._1
    val tiles = board.getTiles

    // Place one in the third tile.
    tiles(2).currentNumber = Some(1)

    // Tiles are calculated using method from top to down and from left to right.
    assert(board.getCandidatesAfterSbaFilter(5).contains(3), "Third tile has one candidate.")
    assert(board.getCandidatesAfterSbaFilter(5).size == 1, "Should contains only one candidate 3")
  }


  "getCombinationsInStr-method" should "return combinations that can be applied to the given " +
    "sub-area and index of the tile." in {
    val file = "src/testingData/9x9_example_board.txt"
    val lines = FileReader.readFile(file)
    val board = FileReader.readFilePuzzleBoardCfg(lines)._1
    val tiles = board.getTiles

    // Tiles are calculated using from top to down and from left to right technic.
    assert(board.getCombinationsInStr(2).containsSlice(Seq[String]("3 + 4 + 8", "3 + 5 + 7", "4 + 5 + 6")),
      "sum of the sub-area is 15 and it contains three tiles.")

    // Place 4 in the third tile.
    tiles(2).currentNumber = Some(4)

    assert(board.getCombinationsInStr(3).containsSlice(Seq[String]("3 + 8", "5 + 6")), "the remaining" +
      "sum is 11 and there are two free tiles left.")

    // Place 1 in the seventh tile.
    tiles(6).currentNumber = Some(1)
    // Sub-area sum is 4 and it contains tiles with indexes 6 and 15. When one is placed in first
    // tile, then there is only one candidate 3 to place in tile with index 15.
    assert(board.getCombinationsInStr(15).contains("3"), "the sum of the subarea is 4 and the only one " +
      "candidate that the user can place is 3.")
    assert(board.getCombinationsInStr(15).length == 1, "The length should be one.")
  }


  // These tests were created to make sure that the Puzzleboard's private methods work properly.

  "getRowCandidates()-method" should "return all possible candidates so that" +
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

  "getLastTwoTilesCombinations()-method" should "return possible combinations of tiles' candidates." in {
    val tile1 = Tile(0, 0, 1)
    val tile2 = Tile(1, 0, 1)
    val targetSum: Int = 4
    val subarea = Subarea(targetSum, Vector(tile1, tile2), tile1)
    val board = Puzzleboard(Vector(tile1, tile2), Vector(subarea))

    val combinations: Buffer[Vector[Int]] = board.getLastTwoTilesCombinations(1, 0, targetSum)
    assert(combinations.size == 1, "Method should return only one combination.")
    assert(combinations.contains(Vector(1, 3)), "The only combination should be '1 + 3'.")
  }

  "getLastTwoTilesCombinations()-method" should "return proper possible combinations of tiles' candidates," +
    "when the sub-area consists of three tiles and one number is already placed in one tile." in {
    val tile1 = Tile(3, 0, 2)
    val tile2 = Tile(3, 1, 2)
    val tile3 = Tile(3, 2, 2)
    var targetSum: Int = 18
    val subarea = Subarea(targetSum, Vector(tile1, tile2, tile3), tile2)
    val board = Puzzleboard(Vector(tile1, tile2, tile3), Vector(subarea))

    tile1.currentNumber = Some(4)
    targetSum -= 4
    val combinations: Buffer[Vector[Int]] = board.getLastTwoTilesCombinations(1, 0, targetSum)
    assert(combinations.size == 2, "Method should return two combinations.")
    assert(combinations.contains(Vector(5, 9)) && combinations.contains(Vector(6, 8)), s"${combinations} did not equal Buffer('5 + 9', '6 + 8')")
  }

  "getLastTwoTilesCombinations()-method" should "return proper possible combinations of tiles' candidates," +
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
    val combinations: Buffer[Vector[Int]] = board.getLastTwoTilesCombinations(3, 0, targetSum)

    // 1 + 9, 2 + 8, 3 + 7, 4 + 6 (because 8 and 6 are placed, combinations 2 + 8 and 4 + 6 should be ignored.)
    assert(combinations.size == 2, "Method should return four possible combinations.")
    assert(combinations.contains(Vector(1, 9)), s"combinations should contain '1 + 9'.")
    assert(combinations.contains(Vector(3, 7)), s"combinations should contain '3 + 7'.")

  }


  "getLastThreeTilesCombinations()-method" should "return possible combinations of tiles' candidates" in {
    val tile1 = Tile(0, 0, 1)
    val tile2 = Tile(1, 0, 1)
    val tile3 = Tile(0, 1, 1)
    val targetSum = 8
    val subarea = Subarea(targetSum, Vector(tile1, tile2, tile3), tile1)
    val board = Puzzleboard(Vector(tile1, tile2, tile3), Vector(subarea))

    val combinations: Buffer[Vector[Int]] = board.getLastThreeTilesCombinations(1, 0, targetSum)
    assert(combinations.contains(Vector(1, 2, 5)) && combinations.contains(Vector(1, 3, 4)), s"${combinations} did not equal Buffer('1 + 2 + 5', '1 + 3 + 4').")
    assert(combinations.size == 2, "The amount of combinations is not the same.")
  }


  "Neigboring sub-areas" should "have different color" in {
    val file = "src/testingData/9x9_example_board.txt"
    val lines = FileReader.readFile(file)

    val board = FileReader.readFilePuzzleBoardCfg(lines.toSeq)._1
    val allSba = board.getSubareas
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

