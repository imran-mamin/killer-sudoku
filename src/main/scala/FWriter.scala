package sudoku

import java.io.*


object FWriter:
  // def eraseFile(file: String): Unit = ???
  def writeFile(filename: String, board: Puzzleboard): Unit =
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))


    bw.close()
    ???

end FWriter

