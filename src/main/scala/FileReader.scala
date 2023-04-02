package sudoku

import scala.io.Source
import java.io.{FileNotFoundException, IOException, BufferedReader}
import scala.collection.mutable.Buffer

def initializeTiles(col: Int, row: Int) =
  val tiles: Buffer[Tile] = Buffer()

  for i <- 0 until row do
    for j <- 0 until col do
      val square: Int = (i / 3) * 3 + (j / 3)
      tiles += new Tile(j, i, square)

    end for
  end for
  tiles

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
    try
      val bufferedSource = Source.fromFile(file)
      val allLines: Seq[String] = bufferedSource.getLines().toSeq
      bufferedSource.close()
      allLines;
    catch
      case e: FileNotFoundException =>
        println("Couldn't find the file.")
        throw e
      case e: IOException           =>
        println("IOException!")
        throw e


  // def readFilePuzzleBoardHistory(): Seq[String] = ???
  // def readFilePuzzleBoardHistory(file: String, history: PuzzleBoardHistory): Seq[String] = ???


  // This is a helper method, which creates tiles and returns Buffer[Tile].
  private def createTiles(tileInStr: Buffer[String], squares: Buffer[Int]): Buffer[Tile] =
    val tiles: Buffer[Tile] = Buffer()
    for i <- tileInStr.indices do
      val tileCode: String = tileInStr(i)
      tiles += this.createTile(tileCode, squares(i))

      // Tile(column: Int, row: Int, square: Int):

    end for
    tiles



  // Creates a single tile object
  private def createTile(strTile: String, square: Int): Tile =
    new Tile(strTile(0) - 'a', strTile(1).toInt - 1, square)



  /** Takes as a parameter Buffer[String], which contains information about subarea.
   * This method will return (Subarea, Buffer[Tile]). */
  private def createSubareaAndTiles(data: Buffer[String]): (Subarea, Buffer[Tile]) =
    var sum: Option[Int] = None
    var amountOfTiles: Option[Int] = None
    var tileSum: Option[String] = None
    var tilesInStr: Buffer[String] = Buffer()
    var squares: Buffer[Int] = Buffer()
    var tiles: Buffer[Tile] = Buffer()


    for i <- data.indices do
      var currentLine = data(i).trim.toLowerCase.replaceAll(" ", "")

      if currentLine.startsWith("sum:") then
          sum = currentLine.drop(4).toIntOption
      else if currentLine.contains("amountoftiles:") then
          amountOfTiles = currentLine.drop(14).toIntOption
      else if currentLine.contains("tilesum:") then
          tileSum = Some(currentLine.drop(8)) // Single tile, which contains information about the sum of subarea
      else if currentLine.contains("tiles:") then
          tilesInStr = tilesInStr ++= currentLine.drop(6).split(",").toBuffer
      else if currentLine.contains("squares:") then
          squares = squares ++= currentLine.drop(8).split(",").map( str => str.toInt ).toBuffer
      else
          println("Not found error")
          assert(false)
    end for

    if sum.isEmpty || amountOfTiles.isEmpty || tileSum.isEmpty
      || tilesInStr.isEmpty || squares.isEmpty || squares.length != tilesInStr.length then
      println("Not all information is provided!")
    end if

    // Index of tile, which contains information about subarea sum
    val indexOfSumTile = tilesInStr.indexOf(tileSum.get)
    // Set up all tiles in the given subarea.
    tiles = tiles ++= this.createTiles(tilesInStr, squares)
    val subarea = new Subarea(sum.get, tiles.toVector, tiles(indexOfSumTile))
    // Subarea(targetSum: Int, tiles: Vector[Tile], tileWithTargetSum: Tile):
    (subarea, tiles)




  // TODO: Add more descriptive error messages
  /** This method takes all the lines of the text file as an input and
   * creates a Puzzleboard-object according to the information given in
   * the text file. */
  def readFilePuzzleBoardCfg(cfg: Seq[String]): Puzzleboard =

    // Will contain the stripped data after handling some of it.
    var stripped: Buffer[String] = cfg.toBuffer.filter( element => element != "" ).map( element => element.trim.toLowerCase)

    val rowNString: Option[String] = stripped.dropWhile( str => !str.contains("#rowsize") ).headOption
    val colNString: Option[String] = stripped.dropWhile( str => !str.contains("#colsize") ).headOption
    var rowN: Option[Int] = Some(9)
    var colN: Option[Int] = Some(9)

    if rowNString.isDefined then
      rowN = rowNString.get.replaceAll(" ", "").split(":")(1).toIntOption

    if colNString.isDefined then
      colN = rowNString.get.replaceAll(" ", "").split(":")(1).toIntOption

    assert(rowN.nonEmpty)
    assert(colN.nonEmpty)
    assert(rowN.get % 3 == 0)
    assert(rowN.get != 0)
    assert(colN.get % 3 == 0)
    assert(colN.get != 0)


    val tiles: Buffer[Tile] = initializeTiles(colN.get, rowN.get)

    val title: Option[String] = stripped.dropWhile( str => !str.contains("#title") ).headOption
    if title.isEmpty then
      println("File does not have a title.")
      assert(false)


    val date: Option[String] = stripped.dropWhile( str => !str.contains("#date") ).headOption
    if date.isEmpty then
      println("No date found!")
      assert(false)

    val subareas: Buffer[Subarea] = Buffer()

    for i <- stripped.indices do
      var current: String = stripped(i).trim.replaceAll(" ", "").toLowerCase

      current match
        case "#subarea:" =>
          // Takes all information in subarea block
          var subAreaInfo: Buffer[String] = stripped.drop(i + 1).takeWhile( str => !str.contains("#") )
          var subareaWithTiles: (Subarea, Buffer[Tile]) = this.createSubareaAndTiles(subAreaInfo)
          subareas += subareaWithTiles._1
          tiles ++= subareaWithTiles._2
        case _          => None


    end for

    new Puzzleboard(tiles.toVector, subareas.toVector)

end FileReader

