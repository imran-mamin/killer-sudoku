package sudoku

import scala.collection.mutable.Buffer

/**
 * Represent the sudoku board, which consists of tiles and sub-areas.
 * @param allTiles is a Vector-data structure, which contains all the Tile-objects that are placed
 *                 in the Puzzleboard.
 * @param subareas is a Vector-data structure, which contains all the Subarea-instances that are
 *                 placed in the Puzzleboard.
 */
class Puzzleboard(allTiles: Vector[Tile], subareas: Vector[Subarea]):


  def getTiles = allTiles

  def getSubareas = subareas
  

  private def getLastTileCombinations(index: Int, targetSum: Int): Buffer[Vector[Int]] =
    val candidates: Buffer[Int] = this.getCandidates(index)
    val possibleNums: Buffer[Vector[Int]] = Buffer()

    for i <- candidates.indices do
      if candidates(i) == targetSum then
        possibleNums += Vector(candidates(i))
      end if
    end for
    possibleNums


  // This method should be private, but is public for unit testing.
  // When the subarea consists of two tiles
  def getLastTwoTilesCombinations(index: Int, subareaIndex: Int, targetSum: Int): Buffer[Vector[Int]] =
    val possibleNums: Buffer[Vector[Int]] = Buffer()
    val tilesInSub: Vector[Tile] = subareas(subareaIndex).getTiles.filter( tile => tile.currentNumber.isEmpty )
    val tile1Candidates: Buffer[Int] = this.getCandidates(allTiles.indexOf(tilesInSub(0)))
    val tile2Candidates: Buffer[Int] = this.getCandidates(allTiles.indexOf(tilesInSub(1)))
    // Calculate possible pairs and adding them to the possiblePairsBuffer.
    for i <- tile1Candidates.indices do
      for j <- tile2Candidates.indices do
        val sum: Int = tile1Candidates(i) + tile2Candidates(j)
        // Tell whether the current pair is in possiblePairs or not.
        val pairIsInAlready: Boolean =
          possibleNums.exists( com => com.contains(tile1Candidates(i)) && com.contains(tile2Candidates(j)) )

        if sum == targetSum && !pairIsInAlready && (tile1Candidates(i) != tile2Candidates(j)) then
          possibleNums += Vector[Int](tile1Candidates(i), tile2Candidates(j))
      end for
    end for
    possibleNums


  // This method should be private, but is public for unit testing.
  def getLastThreeTilesCombinations(index: Int, subareaIndex: Int, targetSum: Int): Buffer[Vector[Int]] =
    val possibleNums: Buffer[Vector[Int]] = Buffer()
    val tilesInSub: Vector[Tile] = subareas(subareaIndex).getTiles.filter( tile => tile.currentNumber.isEmpty )
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
    possibleNums


  private def getFourTilesCombinations(index: Int, subareaIndex: Int, targetSum: Int) =
    val possibleNums: Buffer[Vector[Int]] = Buffer()
    val tilesInSub: Vector[Tile] = subareas(subareaIndex).getTiles
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

  def getCombinationsInStr(index: Int): Buffer[String] =
    val combinations: Buffer[Vector[Int]] = this.getCombinations(index)
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

        
  private def getCombinations(index: Int): Buffer[Vector[Int]] =
      try
        val subareaIndex: Int = allTiles(index).subareaIndex.get
        val amountOfTilesInSub: Int = subareas(subareaIndex).getTiles.size
        val tiles: Vector[Tile] = subareas(subareaIndex).getTiles
        var removedCurrentNum: Option[Int] = None

        // Removes current number for calculations and then returns it back.
        if allTiles(index).currentNumber.isDefined then
          removedCurrentNum = allTiles(index).currentNumber
          allTiles(index).currentNumber = None
        end if

        val notFreeTiles: Int = tiles.count( tile => tile.currentNumber.isDefined )
        var targetSum: Int = subareas(subareaIndex).getTargetSum

        // Removes placed candidates from the subarea's targetSum.
        for i <- tiles.indices do
          if tiles(i).currentNumber.isDefined then
            targetSum -= tiles(i).currentNumber.get
          end if
        end for


        def placeCurrentNumBack(): Unit =
          if removedCurrentNum.isDefined then
            allTiles(index).currentNumber = removedCurrentNum
          end if



        if amountOfTilesInSub == 2 && notFreeTiles == 0 then
          val combinations = getLastTwoTilesCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 3 && notFreeTiles == 0 then
          val combinations = getLastThreeTilesCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 4 && notFreeTiles == 0 then
          val combinations = getFourTilesCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 2 && notFreeTiles == 1 then
          val combinations = getLastTileCombinations(index, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 3 && notFreeTiles == 1 then
          val combinations = getLastTwoTilesCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 4 && notFreeTiles == 1 then
          val combinations = getLastThreeTilesCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 3 && notFreeTiles == 2 then
          val combinations = getLastTileCombinations(index, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 4 && notFreeTiles == 2 then
          val combinations = getLastTwoTilesCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 4 && notFreeTiles == 3 then
          val combinations = getLastTileCombinations(index, targetSum)
          placeCurrentNumBack()
          combinations
        else
          Buffer()

      catch
        case e => throw e


  def getCandidatesAfterSbaFilter(index: Int): Buffer[Int] =
    val combinations: Buffer[Int] = this.getCombinations(index).flatten.distinct
    val candidates = this.getCandidates(index)
    val intersection: Buffer[Int] = Buffer()

    // Intersection of these variables should give candidates after sub-area filter.
    for i <- candidates.indices do
      if combinations.contains(candidates(i)) then
        intersection += candidates(i)
    end for
    intersection


  // This method should be private, but is public for unit testing.
  /**
   * Take index of the tile instance in allTiles vector and filter possible candidates
   * according to already placed numbers in the tiles that are in the same row with this one.
   * @param index of the Tile in allTiles Vector
   * @return Buffer[Int] of possible candidates.
   */
  def getRowCandidates(index: Int): Buffer[Int] =
    val candidates = (1 to 9).toBuffer
    val row: Int = allTiles(index).getRow

    // Removes already placed numbers from the candidates Buffer.
    for i <- allTiles.indices do
      if (allTiles(i).getRow == row) && (allTiles(i).currentNumber.isDefined) then
        candidates -= allTiles(i).currentNumber.get
      end if
    end for
    candidates


  // This method should be private, but is public for unit testing.
  /**
   * Take index of the tile instance in allTiles vector and filters possible candidates
   * according to already placed numbers in the Tiles that are in the same column with this one.
   * @param index of the Tile in allTiles Vector
   * @return Buffer[Int] of possible candidates.
   */
  private def getColCandidates(index: Int): Buffer[Int] =
    val candidates = (1 to 9).toBuffer // initializing all candidates 1-9.
    val column: Int = allTiles(index).getColumn

    // Removes already placed numbers from the candidates Buffer.
    for i <- allTiles.indices do
      if (allTiles(i).getColumn == column) && (allTiles(i).currentNumber.isDefined) then
        candidates -= allTiles(i).currentNumber.get
      end if
    end for
    candidates


  /**
   * Take index of the tile instance in allTiles vector and filters possible candidates
   * according to already placed numbers in the Tiles that are in the same square with this one.
   * @param index of the Tile in allTiles Vector
   * @return Buffer[Int] of possible candidates.
   */
  private def getSquareCandidates(index: Int) =
    val candidates = (1 to 9).toBuffer
    val square: Int = allTiles(index).getSquare

    // Removes already placed numbers from the candidates Buffer.
    for i <- allTiles.indices do
      if (allTiles(i).getSquare == square) && (allTiles(i).currentNumber.isDefined) then
        candidates -= allTiles(i).currentNumber.get
      end if
    end for
    candidates


  // This method should be private, but is public for unit testing.
  /**
   * Take index of the tile (indexation should be same as here in back-end of
   * the program) and gives all the possible candidate numbers that can be placed in this tile instance.
   * This method although gives candidates according to standard sudoku rules, which are the same two
   * numbers can't be placed in the same row, column or square.
   * @param index is the index of the Tile in allTiles Vector.
   * @return Buffer[Int] returns a Buffer of integers that are the possible candidates according to
   *         the standard sudoku rules.
   */
  def getCandidates(index: Int): Buffer[Int] =
    val candidatesRow = this.getRowCandidates(index) // Remaining candidates after row filtering.
    val candidatesCol = this.getColCandidates(index) // Remaining candidates after column filtering.
    val candidatesSqr = this.getSquareCandidates(index) // Remaining candidates after square filtering.

    // Candidates after row and column filtering.
    var intersection = candidatesRow.intersect(candidatesCol)
    // Candidates after also square filtering.
    intersection = intersection.intersect(candidatesSqr)

    intersection

end Puzzleboard
