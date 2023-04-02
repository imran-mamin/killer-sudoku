package sudoku

import scala.io.Source
import java.io.{FileNotFoundException, IOException, BufferedReader}
import scala.collection.mutable.Buffer
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


  private def createTiles(tileInStr: Buffer[String]): Buffer[Tile] =
    val tiles: Buffer[Tile] = Buffer()
    for i <- tileInStr.indices do
      val tileCode: String = tileInStr(i)
      new Tile()
      
    end for
    
    ???
  /** Takes as a parameter Buffer[String], which contains information about subarea.
   * This method will return (Subarea, Buffer[Tile]). */
  private def createSubareaAndTiles(data: Buffer[String]): (Subarea, Buffer[Tile]) =
    var sum: Option[Int] = None
    var amountOfTiles: Option[Int] = None
    var tileSum: Option[String] = None
    val tiles: Buffer[Tile] = Buffer()

    for i <- data.indices do
      var currentLine = data(i).trim.toLowerCase.replaceAll(" ", "")

      currentLine match
        case "sum:" =>
          sum = currentLine.drop(4).toIntOption
        case "amountoftiles:" =>
          amountOfTiles = currentLine.drop(14).toIntOption
        case "tilesum:" =>
          var sumTile = currentLine.drop(8)
        case "tiles:" =>
          val tilesInStr: Buffer[String] = currentLine.drop(6).split(",").toBuffer
          tiles ++= this.createTiles(tilesInStr)

    end for

    ???

  /** This method takes all the lines of the text file as an input and
   * creates a Puzzleboard-object according to the information given in
   * the text file. */
  def readFilePuzzleBoard(cfg: Seq[String]): Puzzleboard =
    val subareas: Buffer[Subarea] = Buffer()
    val tiles: Buffer[Tile] = Buffer()

    // Will contain the remaining data after handling some of it.
    var remaining: Buffer[String] = cfg.toBuffer.filter( element => element != "" )
    val start = remaining.headOption
    if !start.contains("Start") then println("File doesn't start in a proper way.")
    remaining.remove(0) // Will remove the first element "#Start"

    val title = remaining.headOption
    remaining.remove(0) // Removing the title from the list of data.

    var indexOfDate: Int = 0
    var currentLine: String = remaining(indexOfDate).replaceAll(" ", "")

    // Finding the date, when file was written
    while !currentLine.contains("#Date") do
      indexOfDate += 1
      currentLine = remaining(indexOfDate).replaceAll(" ", "")
    end while

    val date: String = remaining(indexOfDate + 1)
    remaining.remove(indexOfDate, 2)

    for i <- remaining.indices do
      var current: String = remaining(i).trim.replaceAll(" ", "").toLowerCase

      current match
        case "#subarea" =>
          var subAreaInfo: Buffer[String] = remaining.drop(i + 1).takeWhile( str => !str.contains("#") )
          var subareaWithTiles: (Subarea, Buffer[Tile]) = this.createSubareaAndTiles(subAreaInfo)
          subareas += subareaWithTiles._1
          tiles ++= subareaWithTiles._2

        case "#end"     => None
        case _          => None


    end for
    new Puzzleboard(tiles.toVector, subareas.toVector)

end FileReader

