package sudoku

import scala.collection.mutable.Buffer
import java.awt.Color

class Puzzleboard(allTiles: Vector[Tile], subareas: Vector[Subarea]):
  // var colors: Vector[Color] = Vector(Color(255, 179, 179), Color(255, 77, 77), Color(153, 0, 0), Color(255, 214, 153), Color(255, 153, 0), Color(204, 255, 204), Color(51, 255, 51), Color(0, 128, 0), Color(255, 255, 0), Color(128, 128, 0), Color(230, 230, 255), Color(153, 153, 255), Color(0, 0, 255), Color(0, 0, 102), Color(236, 179, 255), Color(210, 77, 255), Color(57, 0, 77), Color(255, 153, 230), Color(179, 255, 255), Color(26, 255, 255), Color(0, 179, 179))

  def makeGraph(): Unit = ???
  def paintGraph(): Unit = ???

  def showTiles() = allTiles
  def showSubareas() = subareas
  
  /**
   * This method sets a number to the Tile-object, which index in "allTiles"-Vector is given as a
   * parameter of the function.
   *
   * @param tileIndex index of the Tile in the "allTiles"-Vector.
   * @param num the number, which will be placed in the Tile.
   */

  def addNumber(tileIndex: Int, num: Int): Unit =
    allTiles(tileIndex).currentNumber = Some(num)

  /**
   * This method removes a number from the Tile, if there is one.
   * @param tileIndex is an index of a Tile in "allTiles"-Vector.
   */

  def removeNumber(tileIndex: Int): Unit =
    allTiles(tileIndex).currentNumber = None

  /**
   * Returns the number wrapped in an Option-data structure, which is placed in the Tile, if there is
   * one. If there is no number placed in the given Tile-object, then this method will return None.
   * @param tileIndex
   * @return If there is a number, then Some(Int), else None.
   */
  def getTileNumber(tileIndex: Int): Option[Int] =
    allTiles(tileIndex).currentNumber

  /**
   * This method tells us in which subarea given tile is placed using color.
   * @param tileIndex
   * @return If Tile-object has color, then Some(Color), else None
   */

  def getTileColor(tileIndex: Int): Option[Color] =
    allTiles(tileIndex).color

  /**
   * Returns the number wrapped in an Option that tells us, what the sum of the subarea
   * should be.
   * @param tileIndex
   * @return If Tile-object contains targetSum, then Some(Int), else None.
   */

  def getTileTargetSum(tileIndex: Int): Option[Int] =
    allTiles(tileIndex).targetSum


  def oneTilePossibleCombinations(index: Int, targetSum: Int): Buffer[Vector[Int]] =
    val candidates: Buffer[Int] = this.getCandidates(index)
    val possibleNums: Buffer[Vector[Int]] = Buffer()

    for i <- candidates.indices do
      if candidates(i) == targetSum then
        possibleNums += Vector(candidates(i))
      end if
    end for
    possibleNums



  // When the subarea consists of two tiles
  def twoTilesPossibleCombinations(index: Int, subareaIndex: Int, targetSum: Int): Buffer[Vector[Int]] =
    val possibleNums: Buffer[Vector[Int]] = Buffer()
    val tilesInSub: Vector[Tile] = subareas(subareaIndex).showTiles().filter( tile => tile.currentNumber.isEmpty )
    val tile1Candidates: Buffer[Int] = this.getCandidates(allTiles.indexOf(tilesInSub(0)))
    val tile2Candidates: Buffer[Int] = this.getCandidates(allTiles.indexOf(tilesInSub(1)))
    // Calculating possible pairs and adding them to the possiblePairsBuffer.
    for i <- tile1Candidates.indices do
      for j <- tile2Candidates.indices do
        val sum: Int = tile1Candidates(i) + tile2Candidates(j)
        // Tells, whether the current pair is in possiblePairs or not.
        val pairIsInAlready: Boolean =
          possibleNums.exists( com => com.contains(tile1Candidates(i)) && com.contains(tile2Candidates(j)) )

        if sum == targetSum && !pairIsInAlready && (tile1Candidates(i) != tile2Candidates(j)) then
          possibleNums += Vector[Int](tile1Candidates(i), tile2Candidates(j))
      end for
    end for
    possibleNums // .map( (c1, c2) => s"${c1} + ${c2}" )


  def threeTilesPossibleCombinations(index: Int, subareaIndex: Int, targetSum: Int): Buffer[Vector[Int]] =
    val possibleNums: Buffer[Vector[Int]] = Buffer()
    val tilesInSub: Vector[Tile] = subareas(subareaIndex).showTiles().filter( tile => tile.currentNumber.isEmpty )
    val tile1Candidates: Buffer[Int] = this.getCandidates(allTiles.indexOf(tilesInSub(0)))
    val tile2Candidates: Buffer[Int] = this.getCandidates(allTiles.indexOf(tilesInSub(1)))
    val tile3Candidates: Buffer[Int] = this.getCandidates(allTiles.indexOf(tilesInSub(2)))

    for i <- tile1Candidates.indices do
      for j <- tile2Candidates.indices do
        for k <- tile3Candidates.indices do
          val sum: Int = tile1Candidates(i) + tile2Candidates(j) + tile3Candidates(k)


          val pairsIsInAlready: Boolean =
            possibleNums.exists( v => v.contains(tile1Candidates(i)) && v.contains(tile2Candidates(j)) && v.contains(tile3Candidates(k)) )
          val numsAreNotSame: Boolean =
            tile1Candidates(i) != tile2Candidates(j) &&
            tile2Candidates(j) != tile3Candidates(k) &&
            tile1Candidates(i) != tile3Candidates(k)

          if sum == targetSum && !pairsIsInAlready && numsAreNotSame then
            possibleNums += Vector(tile1Candidates(i), tile2Candidates(j), tile3Candidates(k))
          end if
        end for
      end for
    end for
    possibleNums // .map( v => s"${v(0)} + ${v(1)} + ${v(2)}")


  def fourTilesPossibleCombinations(index: Int, subareaIndex: Int, targetSum: Int) =
    val possibleNums: Buffer[Vector[Int]] = Buffer()
    val tilesInSub: Vector[Tile] = subareas(subareaIndex).showTiles()
    val tile1Candidates: Buffer[Int] = this.getCandidates(allTiles.indexOf(tilesInSub(0)))
    val tile2Candidates: Buffer[Int] = this.getCandidates(allTiles.indexOf(tilesInSub(1)))
    val tile3Candidates: Buffer[Int] = this.getCandidates(allTiles.indexOf(tilesInSub(2)))
    val tile4Candidates: Buffer[Int] = this.getCandidates(allTiles.indexOf(tilesInSub(3)))

    for i <- tile1Candidates.indices do
      for j <- tile2Candidates.indices do
        for k <- tile3Candidates.indices do
          for l <- tile4Candidates.indices do
            val sum: Int = tile1Candidates(i) + tile2Candidates(j) + tile3Candidates(k) + tile4Candidates(l)

            val pairsIsInAlready: Boolean =
            possibleNums.exists( v => v.contains(tile1Candidates(i)) && v.contains(tile2Candidates(j))
              && v.contains(tile3Candidates(k)) && v.contains(tile4Candidates(l)))

            val numsAreNotSame: Boolean =
            tile1Candidates(i) != tile2Candidates(j) &&
            tile2Candidates(j) != tile3Candidates(k) &&
            tile3Candidates(k) != tile4Candidates(l) &&
            tile4Candidates(l) != tile1Candidates(i) &&
            tile4Candidates(l) != tile2Candidates(j) &&
            tile3Candidates(k) != tile1Candidates(i)

            if sum == targetSum && !pairsIsInAlready && numsAreNotSame then
              possibleNums += Vector(tile1Candidates(i), tile2Candidates(j), tile3Candidates(k), tile4Candidates(l))
            end if
          end for
        end for
      end for
    end for

    possibleNums

  def showPossibleCombinationsInStr(index: Int): Buffer[String] =
    val combinations: Buffer[Vector[Int]] = this.showPossibleCombinations(index)
    if combinations.nonEmpty then
      val amountOfElementsInVector: Int = combinations.head.length
      val strBuff = Buffer[String]()
      
      for i <- combinations.indices do
        var str = ""
        for j <- 0 until amountOfElementsInVector do 
          if j == 0 then
            str += combinations(i)(j)
          else
            str += s" + ${combinations(i)(j)}"
        end for
        strBuff += str
      end for
      strBuff
    else
      Buffer()



        
  def showPossibleCombinations(index: Int): Buffer[Vector[Int]] =
      try
        val subareaIndex: Int = allTiles(index).subareaIndex.get
        val amountOfTilesInSub: Int = subareas(subareaIndex).showTiles().size
        val tiles: Vector[Tile] = subareas(subareaIndex).showTiles()
        val notFreeTiles: Int = tiles.count( tile => tile.currentNumber.isDefined )
        var targetSum: Int = subareas(subareaIndex).getTargetSum()

        // Removes placed candidates from the subarea's targetSum.
        for i <- tiles.indices do
          if tiles(i).currentNumber.isDefined then
            targetSum -= tiles(i).currentNumber.get
          end if
        end for

        // TODO: Add handling when some tiles are not free.
        if amountOfTilesInSub == 2 && notFreeTiles == 0 then
          twoTilesPossibleCombinations(index, subareaIndex, targetSum)
        else if amountOfTilesInSub == 3 && notFreeTiles == 0 then
          threeTilesPossibleCombinations(index, subareaIndex, targetSum)
        else if amountOfTilesInSub == 4 && notFreeTiles == 0 then
          fourTilesPossibleCombinations(index, subareaIndex, targetSum)
        else if amountOfTilesInSub == 2 && notFreeTiles == 1 then
          oneTilePossibleCombinations(index, targetSum)
        else if amountOfTilesInSub == 3 && notFreeTiles == 1 then
          twoTilesPossibleCombinations(index, subareaIndex, targetSum)
        else if amountOfTilesInSub == 4 && notFreeTiles == 1 then
          threeTilesPossibleCombinations(index, subareaIndex, targetSum)
        else if amountOfTilesInSub == 3 && notFreeTiles == 2 then
          oneTilePossibleCombinations(index, targetSum)
        else if amountOfTilesInSub == 4 && notFreeTiles == 2 then
          twoTilesPossibleCombinations(index, subareaIndex, targetSum)
        else if amountOfTilesInSub == 4 && notFreeTiles == 3 then
          oneTilePossibleCombinations(index, targetSum)
        else
          Buffer()

      catch
        case e => throw e


  def getCandidatesAfterSbaFilter(index: Int): Buffer[Int] =
    val possibleCombinations: Buffer[Int] = this.showPossibleCombinations(index).flatten.distinct
    val candidates = this.getCandidates(index)
    val intersection: Buffer[Int] = Buffer()

    // Intersection of these variables should give candidates after sub-area filter.
    for i <- candidates.indices do
      if possibleCombinations.contains(candidates(i)) then
        intersection += candidates(i)
    end for
    intersection



  def rowCandidates(index: Int): Buffer[Int] =
    val candidates = (1 to 9).toBuffer
    val row: Int = allTiles(index).getRow

    for i <- allTiles.indices do
      if (allTiles(i).getRow == row) && (allTiles(i).currentNumber.isDefined) then
        candidates -= allTiles(i).currentNumber.get
      end if
    end for
    candidates



  private def colCandidates(index: Int) =
    val candidates = (1 to 9).toBuffer
    val column: Int = allTiles(index).getColumn

    for i <- allTiles.indices do
      if (allTiles(i).getColumn == column) && (allTiles(i).currentNumber.isDefined) then
        candidates -= allTiles(i).currentNumber.get
      end if
    end for
    candidates



  private def squareCandidates(index: Int) =
    val candidates = (1 to 9).toBuffer
    val square: Int = allTiles(index).getSquare

    for i <- allTiles.indices do
      if (allTiles(i).getSquare == square) && (allTiles(i).currentNumber.isDefined) then
        candidates -= allTiles(i).currentNumber.get
      end if
    end for
    candidates


  def getCandidates(index: Int): Buffer[Int] =
    val candidatesRow = this.rowCandidates(index)
    val candidatesCol = this.colCandidates(index)
    val candidatesSqr = this.squareCandidates(index)

    var intersection = candidatesRow.intersect(candidatesCol)
    intersection = intersection.intersect(candidatesSqr)

    intersection

end Puzzleboard
