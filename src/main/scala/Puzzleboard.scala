package sudoku

import scala.collection.mutable.Buffer

/**
 * This class represents the sudoku board, which consists of tiles and sub-areas.
 * @param allTiles is a Vector-data structure, which contains all the Tile-objects that are placed
 *                 in the Puzzleboard.
 * @param subareas is a Vector-data structure, which contains all the Subarea-instances that are
 *                 placed in the Puzzleboard.
 */
class Puzzleboard(allTiles: Vector[Tile], subareas: Vector[Subarea]):


  def getTiles = allTiles

  def getSubareas = subareas
  

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
    val tilesInSub: Vector[Tile] = subareas(subareaIndex).getTiles.filter( tile => tile.currentNumber.isEmpty )
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
    possibleNums // .map( v => s"${v(0)} + ${v(1)} + ${v(2)}")


  def fourTilesPossibleCombinations(index: Int, subareaIndex: Int, targetSum: Int) =
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
          val combinations = twoTilesPossibleCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 3 && notFreeTiles == 0 then
          val combinations = threeTilesPossibleCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 4 && notFreeTiles == 0 then
          val combinations = fourTilesPossibleCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 2 && notFreeTiles == 1 then
          val combinations = oneTilePossibleCombinations(index, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 3 && notFreeTiles == 1 then
          val combinations = twoTilesPossibleCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 4 && notFreeTiles == 1 then
          val combinations = threeTilesPossibleCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 3 && notFreeTiles == 2 then
          val combinations = oneTilePossibleCombinations(index, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 4 && notFreeTiles == 2 then
          val combinations = twoTilesPossibleCombinations(index, subareaIndex, targetSum)
          placeCurrentNumBack()
          combinations
        else if amountOfTilesInSub == 4 && notFreeTiles == 3 then
          val combinations = oneTilePossibleCombinations(index, targetSum)
          placeCurrentNumBack()
          combinations
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


  /**
   * This method takes the index of the Tile-instance in allTiles Vector and filters possible candidates
   * according to already placed numbers in the Tiles that are in the same row with this one.
   * @param index of the Tile in allTiles Vector
   * @return Buffer[Int] of possible candidates.
   */
  def rowCandidates(index: Int): Buffer[Int] =
    val candidates = (1 to 9).toBuffer
    val row: Int = allTiles(index).getRow

    // Removes already placed numbers from the candidates Buffer.
    for i <- allTiles.indices do
      if (allTiles(i).getRow == row) && (allTiles(i).currentNumber.isDefined) then
        candidates -= allTiles(i).currentNumber.get
      end if
    end for
    candidates


  /**
   * This method takes the index of the Tile-instance in allTiles Vector and filters possible candidates
   * according to already placed numbers in the Tiles that are in the same column with this one.
   * @param index of the Tile in allTiles Vector
   * @return Buffer[Int] of possible candidates.
   */
  private def colCandidates(index: Int): Buffer[Int] =
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
   * This method takes the index of the Tile-instance in allTiles Vector and filters possible candidates
   * according to already placed numbers in the Tiles that are in the same square with this one.
   * @param index of the Tile in allTiles Vector
   * @return Buffer[Int] of possible candidates.
   */
  private def squareCandidates(index: Int) =
    val candidates = (1 to 9).toBuffer
    val square: Int = allTiles(index).getSquare

    // Removes already placed numbers from the candidates Buffer.
    for i <- allTiles.indices do
      if (allTiles(i).getSquare == square) && (allTiles(i).currentNumber.isDefined) then
        candidates -= allTiles(i).currentNumber.get
      end if
    end for
    candidates


  /**
   * This method takes the index of the Tile (indexation should be same as here in back-end of
   * the program) and gives all the possible candidate numbers that can be placed in this Tile-instance.
   * This method although gives candidates according to standard sudoku rules, which are the same two
   * numbers can't be placed in the same row, column or square.
   * @param index is the index of the Tile in allTiles Vector.
   * @return Buffer[Int] returns a Buffer of integers that are the possible candidates according to
   *         the standard sudoku rules.
   */
  def getCandidates(index: Int): Buffer[Int] =
    val candidatesRow = this.rowCandidates(index) // Remaining candidates after row filtering.
    val candidatesCol = this.colCandidates(index) // Remaining candidates after column filtering.
    val candidatesSqr = this.squareCandidates(index) // Remaining candidates after square filtering.

    // Possible candidates after row and column filtering.
    var intersection = candidatesRow.intersect(candidatesCol)
    // Possible candidates after also square filtering.
    intersection = intersection.intersect(candidatesSqr)

    intersection

end Puzzleboard
