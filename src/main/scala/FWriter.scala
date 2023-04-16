package sudoku

import java.io.*


object FWriter:
  // def eraseFile(file: String): Unit = ???
  def writeFile(filename: String, directory: File, board: Puzzleboard): Unit =
    try
      val file = new File(directory, filename)
      val bw = new BufferedWriter(new FileWriter(file))
      // Writes the title of the puzzle (in this case the filename)
      bw.write("#Title " + filename)

      bw.close()
    catch
      case e => throw e


end FWriter

