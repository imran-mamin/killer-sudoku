package sudoku

import scala.io.Source
import java.io.{FileNotFoundException, IOException, BufferedReader}
import scala.collection.mutable.Buffer
import java.awt.Color

def initializeTiles(col: Int, row: Int) =
  val tiles: Buffer[Tile] = Buffer()

  for i <- 0 until row do
    for j <- 0 until col do
      val square: Int = (i / 3) * 3 + (j / 3)
      tiles += new Tile(j, i, square)

      for k <- tiles.indices do
        val x: Int = tiles(k).getRow
        val y: Int = tiles(k).getColumn
        val north: (Int, Int) = (x - 1, y)
        val south: (Int, Int) = (x + 1, y)
        val east: (Int, Int) = (x, y + 1)
        val west: (Int, Int) = (x, y - 1)

        // Add neighbors to tiles
        if north._1 >= 0 && !tiles(k).neighbors.contains(tiles((x - 1) * col + y)) then
          tiles(k).neighbors += tiles((x - 1) * col + y)
        if east._2 < col && (tiles.length - 1 >= (x * col + y + 1)) &&
          !tiles(k).neighbors.contains(tiles(x * col + y + 1)) then
          tiles(k).neighbors += tiles(x * col + y + 1)
        if south._1 < row && (tiles.length - 1 >= ((x + 1) * col + y)) &&
          !tiles(k).neighbors.contains(tiles((x + 1) * col + y)) then
          tiles(k).neighbors += tiles((x + 1) * col + y)
        if west._2 >= 0 && !tiles(k).neighbors.contains(tiles(x * col + y - 1)) then
          tiles(k).neighbors += tiles(x * col + y - 1)
      end for
    end for
  end for
  tiles



def convertHSBtoRGB(hsbColor: Color): (Int, Int, Int) =
  val rgb = hsbColor.getRGB
  val red = (rgb >> 16) & 0xFF
  val green = (rgb >> 8) & 0xFF
  val blue = rgb & 0xFF
  (red, green, blue)


/**
 * This object has methods that reads the received file from the user input in the gui
 * and first divides it into a Buffer of lines and checks if there are errors in the
 * provided file. If the file meets all the requirements then with the help of this
 * object's methods the Puzzleboard-instance is initialized.
 */

object FileReader:

  /**
   * Reads file content and returns all the lines respectively.
   * @return Seq[String], which contains all the lines in the given file.
   */
  def readFile(file: String): Seq[String] =
    val bufferedSource = Source.fromFile(file)
    val allLines: Seq[String] = bufferedSource.getLines().toSeq
    bufferedSource.close()
    allLines


  // This is a helper method, which updates tiles and returns Buffer[Tile].
  private def updateTiles(tileInStr: Buffer[String], allTiles: Buffer[Tile]): Buffer[Tile] =
    val tiles: Buffer[Tile] = Buffer()
    val chars: Vector[Char] = ('a' to 'z').toVector

    for i <- tileInStr.indices do
      val tileCode: String = tileInStr(i)
      val (row, col) = this.findRowAndColumn(tileCode)
      tiles += allTiles.find( tile => tile.getRow == row && tile.getColumn == col ).get
      assert(!tiles.last.inUse, s"Tile column: ${chars(col)}, row: ${row}, duplicate found.")
      tiles.last.inUse = true
    end for
    tiles


  // Finds row and column of the given tile string
  private def findRowAndColumn(strTile: String): (Int, Int) =
    val (column, row) = (strTile(0) - 'a', strTile(1).asDigit - 1)
    (row, column)


  private def checkForKeyDuplicatesInSubareaInfo(linesWithoutSpace: Buffer[String]): Unit =
    var sumCount: Int = 0
    var amountoftilesCount: Int = 0
    var tilesumCount: Int = 0
    var tilesCount: Int = 0
    var squaresCount: Int = 0

    for i <- linesWithoutSpace.indices do
      val currentLine: String = linesWithoutSpace(i)
      if currentLine.startsWith("sum:") then
        sumCount += 1
      else if currentLine.contains("amountoftiles:") then
        amountoftilesCount += 1
      else if currentLine.contains("tilesum:") then
        tilesumCount += 1
      else if currentLine.contains("tiles:") then
        tilesCount += 1
      else if currentLine.contains("squares:") then
        squaresCount +=1
    end for
    assert(sumCount <= 1, s"Duplicate found: '${linesWithoutSpace.find( line => line.startsWith("sum") ).getOrElse(None)}'")
    assert(amountoftilesCount <= 1, s"Duplicate found: '${linesWithoutSpace.find( line => line.startsWith("amountoftiles") ).getOrElse(None)}'")
    assert(tilesumCount <= 1, s"Duplicate found: '${linesWithoutSpace.find( line => line.startsWith("tilesum") ).getOrElse(None)}'")
    assert(tilesCount <= 1, s"Duplicate found: '${linesWithoutSpace.find( line => line.startsWith("tiles") ).getOrElse(None)}'")
    assert(squaresCount <= 1, s"Duplicate found: '${linesWithoutSpace.find( line => line.startsWith("squares") ).getOrElse(None)}'")


  /** Takes as a parameter Buffer[String], which contains information about subarea.
   * This method will return (Subarea, Buffer[Tile]). */
  private def createSubareaAndUpdateTiles(data: Buffer[String], allTiles: Buffer[Tile]): Subarea =
    var sum: Option[Int] = None
    var amountOfTiles: Option[Int] = None
    var tileSum: Option[String] = None
    var tilesInStr: Buffer[String] = Buffer()
    var squares: Buffer[Int] = Buffer()
    var tiles: Buffer[Tile] = Buffer()
    // These are used for corrupted file, when duplicate info occur.
    val linesWithoutSpace: Buffer[String] = data.map( line => line.trim.toLowerCase.replaceAll(" ", "") )
    checkForKeyDuplicatesInSubareaInfo(linesWithoutSpace)

    // TODO: Add NumberFormatException handling, when user doesn't provide digits after ':'.
    for i <- linesWithoutSpace.indices do
      var currentLine = linesWithoutSpace(i)

      if currentLine.startsWith("sum:") then
          sum = currentLine.drop(4).toIntOption
      else if currentLine.contains("amountoftiles:") then
          amountOfTiles = currentLine.drop(14).toIntOption
      else if currentLine.contains("tilesum:") then
          tileSum = Some(currentLine.drop(8)) // Single tile, which contains information about the sum of subarea
      else if currentLine.contains("tiles:") then
          tilesInStr ++= currentLine.drop(6).split(",").toBuffer
      else if currentLine.contains("squares:") then
          squares ++= currentLine.drop(8).split(",").map( str => str.toInt ).toBuffer
      else
          // println("Not found error")
          assert(false, "Not found error!")
    end for

    if sum.isEmpty || amountOfTiles.isEmpty || tileSum.isEmpty
      || tilesInStr.isEmpty || squares.isEmpty || squares.length != tilesInStr.length then
      // println("")
      assert(false, "Not all information is provided!")
    end if

    // Index of tile, which contains information about subarea sum
    val indexOfSumTile = tilesInStr.indexOf(tileSum.get)
    // Set up all tiles in the given subarea.
    tiles = this.updateTiles(tilesInStr, allTiles)

    // Set sum of the subarea to one of the tiles
    tiles(indexOfSumTile).targetSum = Some(sum.get)

    val subarea = new Subarea(sum.get, tiles.toVector, tiles(indexOfSumTile))
    // Subarea(targetSum: Int, tiles: Vector[Tile], tileWithTargetSum: Tile):
    subarea


/** This method will add an index of the subarea to every tile, which is
 * in this subarea. */

  def addSubareaIndexToTiles(puzzleboard: Puzzleboard): Unit =
    val subareas = puzzleboard.getSubareas
    val allTilesSet: Set[Tile] = puzzleboard.getTiles.toSet

    for i <- subareas.indices do
      val tilesInSubarea: Vector[Tile] = subareas(i).getTiles

      for j <- tilesInSubarea.indices do
        // Add subarea index to the single tile
        tilesInSubarea(j).subareaIndex = Some(i)
      end for
    end for


  // Will look something like this in the file: #placedNums:
  // a1: 6, a8: 8, ...
  def setCurrentNumsToTiles(placedNums: Buffer[String], allTiles: Buffer[Tile]): Unit =
    val withoutTagAndSpaces: Buffer[String] = placedNums.map( str => str.replaceAll(" ", "") ).dropWhile( str => !str.contains(":") ).drop(1) // Last is the sign ':' itself.
    val str: String = withoutTagAndSpaces.mkString(",") //.reduceLeft( (first, second) => first + second ).replaceAll(" ", "")
    val pairTileAndNumInStr: Array[Array[String]] = str.split(',').map( str => str.split(':') )
    pairTileAndNumInStr.foreach( n => n.foreach( m => println(m) ) )
    assert(pairTileAndNumInStr.forall( arr => arr.length == 2 ))

    for i <- pairTileAndNumInStr.indices do
      val tileIndexStr: String = pairTileAndNumInStr(i)(0)
      val num: Int = pairTileAndNumInStr(i)(1).toInt
      val (row, col): (Int, Int) = this.findRowAndColumn(tileIndexStr)

      allTiles.find( tile => tile.getRow == row && tile.getColumn == col ).get.currentNumber = Some(num)

    end for


  /**
   * This method adds neighbors to all sub-areas on the board.
   */
  def addNeighborsToSubareas(puzzle: Puzzleboard): Unit =
    val tiles: Vector[Tile] = puzzle.getTiles
    val subareas: Vector[Subarea] = puzzle.getSubareas


    for i <- tiles.indices do
      // This two variables will be used, when comparing two Tile-instances below.
      val neighbors: Vector[Tile] = tiles(i).neighbors.toVector // neighbors of the tile(i).
      val cSubIndex: Int = tiles(i).subareaIndex.get // sub-area index of this tile.


      for j <- neighbors.indices do
        val nSubIndex: Int = neighbors(j).subareaIndex.get

        if nSubIndex != cSubIndex then
          if !subareas(nSubIndex).neighbors.contains(subareas(cSubIndex)) then
            subareas(nSubIndex).neighbors = subareas(nSubIndex).neighbors :+ subareas(cSubIndex)
          end if
          if !subareas(cSubIndex).neighbors.contains(subareas(nSubIndex)) then
            subareas(cSubIndex).neighbors = subareas(cSubIndex).neighbors :+ subareas(nSubIndex)
          end if
        end if
      end for
    end for



  /** This method adds colors to sub-areas using Greedy-algorithm. */
  def addColorToSubareas(puzzle: Puzzleboard): Unit =
    val subareas: Vector[Subarea] = puzzle.getSubareas
    val colors: Buffer[Color] = Buffer()

    for i <- subareas.indices do
      val currentSba = subareas(i)
      if currentSba.color.isEmpty then
        val neighbors: Vector[Subarea] = currentSba.neighbors
        val colorInNeighbors: Buffer[Color] = Buffer()
        // Adds colors that neighboring sub-areas have into colorInNeighbors-buffer.
        // TODO: Use Set instead of Buffer[Color]
        neighbors.foreach( neighbor =>
          if neighbor.color.isDefined && !colorInNeighbors.contains(neighbor.color.get) then
            colorInNeighbors += neighbor.color.get )

        val remainingColors: Buffer[Color] = colors.diff(colorInNeighbors)
        if remainingColors.nonEmpty then
          currentSba.color = Some(remainingColors.head) // Takes the first color from possible colors.
        else
          colors += Color.getHSBColor((Math.PI / i + 1).toFloat, 60, 100)
          currentSba.color = Some(colors.last)
      end if
    end for


  // TODO: Add more descriptive error messages
  /** This method takes all the lines of the text file as an input and
   * creates a Puzzleboard-object according to the information given in
   * the text file. */
  def readFilePuzzleBoardCfg(cfg: Seq[String]): (Puzzleboard, Int, Int, String) =

    // Will contain the stripped data after handling some of it.
    var stripped: Buffer[String] = cfg.toBuffer.filter( element => element != "" ).map( element => element.trim.toLowerCase)

    val rowNString: Option[String] = stripped.dropWhile( str => !str.contains("#rowsize") ).headOption
    val colNString: Option[String] = stripped.dropWhile( str => !str.contains("#colsize") ).headOption
    var rowN: Option[Int] = Some(9)
    var colN: Option[Int] = Some(9)

    if rowNString.isDefined then
      rowN = rowNString.get.replaceAll(" ", "").split(":").last.toIntOption

    if colNString.isDefined then
      colN = colNString.get.replaceAll(" ", "").split(":").last.toIntOption

    if rowN.isEmpty then
      assert(rowN.nonEmpty, "Amount of rows not specified!")
    if colN.isEmpty then
      assert(colN.nonEmpty, "Amount of columns not specified!")
    if rowN.getOrElse(1) % 3 != 0 then
      assert(rowN.get % 3 == 0, "Amount of rows are not divisible by three!")
    if rowN.getOrElse(0) == 0 then
      assert(rowN.get != 0, "Amount of rows cannot be 0!")
    if colN.getOrElse(1) % 3 != 0 then
      assert(colN.get % 3 == 0, "Amount of columns are not divisible by three!")
    if colN.getOrElse(0) == 0 then
      assert(colN.get != 0, "There cannot be 0 columns!")


    val tiles: Buffer[Tile] = initializeTiles(colN.get, rowN.get)

    val titleOpt: Option[String] = stripped.dropWhile( str => !str.contains("#title") ).headOption
    if titleOpt.isEmpty then
      // println("File does not have a title.")
      assert(false, "File does not have a title.")

    val title: String = titleOpt.get.trim.drop(6).trim

    val subareas: Buffer[Subarea] = Buffer()

    for i <- stripped.indices do
      var current: String = stripped(i).trim.replaceAll(" ", "").toLowerCase

      current match
        case "#subarea:" =>
          // Takes all information in subarea block
          var subAreaInfo: Buffer[String] = stripped.drop(i + 1).takeWhile( str => !str.contains("#") )
          subareas += this.createSubareaAndUpdateTiles(subAreaInfo, tiles)
        case _          => None

    end for

    var i = -1
    tiles.foreach( tile => if !tile.inUse then i = tiles.indexOf(tile) )
    assert(i == -1, s"Tile row: ${tiles(i).getRow} and column: ${tiles(i).getColumn} info is missing in config file.")

    // Place numbers in the tiles that are specified in the provided file.
    if stripped.exists( str => str.contains("#placednums") ) then
      val placedNums: Buffer[String] = stripped.dropWhile( str => !str.contains("#placednums") )  //.takeWhile( str => !str.contains("#") )
      placedNums(0) = placedNums.head.trim.replace("#placednums", "placednums")
      val placedNumsWithoutHash = placedNums.takeWhile( str => !str.contains("#") )
      setCurrentNumsToTiles(placedNumsWithoutHash, tiles)
    end if

    val puzzle = new Puzzleboard(tiles.toVector, subareas.toVector)
    addSubareaIndexToTiles(puzzle)
    addNeighborsToSubareas(puzzle)
    addColorToSubareas(puzzle)
    (puzzle, rowN.get, colN.get, title)

end FileReader

