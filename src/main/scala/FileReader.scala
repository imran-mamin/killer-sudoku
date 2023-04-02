package sudoku

import scala.io.Source
import java.io.{FileNotFoundException, IOException, BufferedReader}

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


  def readFilePuzzleBoardHistory(): Seq[String] = ???
  // def readFilePuzzleBoardHistory(file: String, history: PuzzleBoardHistory): Seq[String] = ???
  // def readFilePuzzleBoard(cfg: Seq[String]): Puzzleboard = ???
end FileReader

