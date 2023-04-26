package sudoku

import java.io.*


object FWriter:

  /**
   * This method converts row and column integers into tileIndex in String.
   * @param row of the tile
   * @param col of the tile
   * @return tileIndex in String, for example "a1"
   */
  private def rowColConverter(row: Int, col: Int): String =
    val letters: Vector[Char] = ('a' to 'z').toVector
    letters(col) + (row + 1).toString

  // This method returns a string of the tiles, for example "tiles: e9, f9, g9".
  private def tilesString(tiles: Vector[Tile]): String =
    var str = "tiles: "
    val tileIndexAsString = tiles.map( tile => rowColConverter(tile.getRow, tile.getColumn) )
    str += tileIndexAsString.mkString(", ")
    str

  // This method returns a string of squares, for example "squares: 2, 2, 4"
  private def squaresString(tiles: Vector[Tile]): String =
    var str = "squares: "
    val squares: Vector[Int] = tiles.map( tile => tile.getSquare )
    str += squares.mkString(", ")
    str


  // Writes file to the given directory
  def writeFile(filename: String, directory: File, board: Puzzleboard, rowNSize: Int, colNSize: Int): Unit =
    try
      val file = new File(directory, filename)
      val bw = new BufferedWriter(new FileWriter(file))
      // Writes the title of the puzzle (in this case the filename)
      bw.write(s"#Title ${filename}\n")
      bw.write(s"#colsize: ${colNSize}\n")
      bw.write(s"#rowsize: ${rowNSize}\n\n")

      val subareas: Vector[Subarea] = board.getSubareas

      for i <- subareas.indices do
        val tilesInSub: Vector[Tile] = subareas(i).getTiles
        val tileWithTargetSum: Tile = tilesInSub.dropWhile( tile => tile.targetSum.isEmpty ).head

        bw.write(s"#Subarea:\n")
        bw.write(s"sum: ${subareas(i).getTargetSum}\n")
        bw.write(s"amountOfTiles: ${tilesInSub.length}\n")
        bw.write(s"tileSum: ${rowColConverter(tileWithTargetSum.getRow, tileWithTargetSum.getColumn)}\n")
        bw.write(this.tilesString(tilesInSub) + "\n")
        bw.write(this.squaresString(tilesInSub) + "\n\n")
      end for
      val tilesWithNums: Vector[Tile] = board.getTiles.filter( tile => tile.currentNumber.isDefined )

      if tilesWithNums.nonEmpty then
        bw.write("#placedNums: \n")

        for j <- tilesWithNums.indices do
          bw.write(s"${this.rowColConverter(tilesWithNums(j).getRow, tilesWithNums(j).getColumn)}: ${tilesWithNums(j).currentNumber.get}\n")
        end for
      end if

      bw.close()
    catch
      case e => throw e


end FWriter

