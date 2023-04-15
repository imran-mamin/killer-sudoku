package GUI


import javafx.beans.property.ReadOnlyObjectProperty
import javafx.geometry.{HPos, VPos}
import javafx.scene.control
import javafx.scene.control.{Alert, ButtonType, ChoiceDialog, ContextMenu, ListView, MenuButton, TextField}
import javafx.scene.control.Alert.AlertType
import javafx.scene.layout.{GridPane, VBox}
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.{Background, Border, Pane, StackPane, TilePane}
import scalafx.scene.control.{Button, Label, Menu, MenuBar, MenuItem, SeparatorMenuItem}
import scalafx.Includes.*
import scalafx.geometry.Pos
import scalafx.scene.canvas.*
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.text.{Text, Font}
import javafx.geometry.Insets
import scalafx.scene.Group
import scalafx.scene.layout.Region

import scala.collection.mutable.Buffer
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.control.{TextInputDialog, ToolBar}
import javafx.scene.shape.{Circle, Line}
import javafx.scene.text.TextAlignment
import javafx.stage.FileChooser
import sudoku.*

import java.awt.{Cursor, TextArea}
import java.io.FileInputStream


object Main extends JFXApp3:


  def start(): Unit =
    stage = new JFXApp3.PrimaryStage:
      title = "Killer-Sudoku"
      width = 800
      height = 600

    val root = Pane()
    // grid.layoutY = 80
    // grid.layoutX = 80
    val gridWith3x3Squares = GridPane()
    gridWith3x3Squares.layoutY = 80
    gridWith3x3Squares.layoutX = 80


    var puzzleboard: Option[Puzzleboard] = None
    val previousFiles = Buffer[String]()

    val mainScene = Scene(parent = root)
    stage.scene = mainScene

    val tiles: Buffer[Rectangle] = Buffer()

    var gridWith9TilesInsets: Insets = Insets.EMPTY

    // TODO: Fix subarea sum placement to count border with.
    def create9Tiles(): GridPane =
      val gridWith9Tiles = GridPane()

      for i <- 0 until 3 do
        for j <- 0 until 3 do
          val rectangle = new Rectangle:
            width = 40
            height = 40
          /* val text = new Text("22")
          // text.setFont(Font.font("Arial", FontWeight.BOLD, 14))
          text.setFill(Color.Yellow)
          text.setX(rectangle.getX() + 10)
          text.setY(rectangle.getY() + 20) */
          rectangle.setStroke(Color.Black)
          rectangle.setStrokeWidth(0.5)
          tiles += rectangle
          gridWith9Tiles.add(rectangle, j + 1, i + 1)
          // gridWith9Tiles.add(text, j + 1, i + 1)
      gridWith9Tiles.border = Border.stroke(Color.Black)
      gridWith9TilesInsets = gridWith9Tiles.getBorder.getInsets
      // println("gridWith9Tiles Insets: " + gridWith9Tiles.getBorder.getInsets)
      gridWith9Tiles


    val characters: List[Char] = ('a' to 'z').toList

    // Sets coordinates of each tile
    def setNumAndCharAsPos(row: Int, col: Int) =
      for i <- 0 until row do // equals y-coordinate

         // Displaying the y-coordinates in the GUI window
        val colText = new Label:
          text = " " + (i + 1) + " "
        colText.layoutX = 60
        // Starting point plus amount of tiles multiplied by their width
        colText.layoutY = 95 + i * 41.5
        root.children += colText

        for j <- 0 until col do // equals x-coordinate
          if i == 0 then
            val row = new Label:
              text = characters(j).toString
            row.layoutX = 100 + j * 41.5
            row.layoutY = 60
            root.children += row
          end if
        end for
      end for



    // Set coordinates in the back-end of the program.
    def setCoordinates(board: Puzzleboard, row: Int, col: Int) =
      val boardTiles = board.showTiles()
      for i <- 0 until row do
        for j <- 0 until col do
          val borderLeftOf9x9Square: Double = gridWith3x3Squares.getBorder.getInsets.getLeft
          val borderLeftOf3x3Square: Double = gridWith9TilesInsets.getLeft
          val tileBorder: Double = tiles.head.getStrokeWidth

          // X-coordinate is layoutX of 9x9 Square + tilesWidth * amountOfTiles before this one +
          // the border of 9x9 square + borders of 3x3 squares + single tile border.
          boardTiles((i * col) + j).xCoord = gridWith3x3Squares.getLayoutX + j * tiles.head.getWidth +
            borderLeftOf9x9Square + ((j / 3) * 2 + 1) * borderLeftOf3x3Square + (2 * j + 1) * tileBorder

          val borderTopOf9x9Square: Double = gridWith3x3Squares.getBorder.getInsets.getTop
          val borderTopOf3x3Square: Double = 2
          boardTiles((i * col) + j).yCoord = gridWith3x3Squares.getLayoutY + i * tiles.head.getHeight +
            borderTopOf9x9Square
          // boardTiles((i * col) + j).edgeSize = tiles(0).getWidth

        end for
      end for



    def create3x3Squares(row: Int, col: Int, board: Puzzleboard) =
      val amountOfSquaresHorizontal: Int = col / 3
      val amountOfSquaresVertical: Int = row / 3

      for i <- 0 until amountOfSquaresVertical do  // rows (y)
        for j <- 0 until amountOfSquaresHorizontal do  // columns (x)
          gridWith3x3Squares.add(create9Tiles(), j, i)
          gridWith3x3Squares.border = Border.stroke(Color.Black)
        end for
      end for
      setCoordinates(board, row, col)

      // This method will rearrange tiles, so that the order of them will be the same as in back-end
      // rearrangeTilesAsInBackEnd(row: Int, col: Int)
      // board.showTiles().foreach( tile => println("xCoord: " + tile.xCoord + " yCoord: " + tile.yCoord) )
      root.children += gridWith3x3Squares
      setNumAndCharAsPos(row, col)



    val allListViews: Buffer[ListView[String]] = Buffer()


    def initializeListViews(board: Puzzleboard, row: Int, col: Int): Unit =
      val amountOfSquaresHorizontal: Int = col / 3
      val amountOfSquaresVertical: Int = row / 3
      val boardTiles = board.showTiles()

      // This method converts index of UI into the index in back-end.
      def convertIndex(i: Int): Int =
        val m = (i / 9) / amountOfSquaresHorizontal // row of the 3x3 square
        val n = (i / 9) % amountOfSquaresHorizontal // col of the 3x3 square
        val topLeft3x3 = m * amountOfSquaresHorizontal * 3 * 3 + n * 3 // index of the tile in top left corner in 3x3square
        ((i % 9) / 3) * amountOfSquaresHorizontal * 3 + (i % 9) % 3 + topLeft3x3



      for j <- tiles.indices do
        val currentListView = new ListView[String]()
        currentListView.setMaxSize(100, 100)
        currentListView.visible = false
        allListViews += currentListView
        // val stack = new StackPane(currentListView, tiles(j))
        root.children += currentListView

        currentListView.getItems.add("")
        // val candidates = board.getCandidates(convertIndex(j)) // boardTiles(convertIndex(j)).candidates
        val candidates = board.getCandidatesAfterSbaFilter(convertIndex(j))

        for k <- candidates.indices do
          currentListView.getItems.add(candidates(k).toString)
        end for

        val xCoord = boardTiles(convertIndex(j)).xCoord
        val yCoord = boardTiles(convertIndex(j)).yCoord
       //  println(s"xCoord: ${xCoord}, yCoord: ${yCoord}")
        currentListView.layoutX = xCoord
        currentListView.layoutY = yCoord + 20
      end for


    val texts = Buffer[Text]()

    def initializeTextInTiles(): Unit =
      for j <- tiles.indices do
        val text = new Text("")
        text.visible = false
        texts += text
        root.children += text
      end for



     // Possible combinations of the tiles in subarea
    val possibleComLabel = new Label("Possible combinations")
    // possibleComLabel.resize(20, 10)
    possibleComLabel.setFont(new Font(18))
    possibleComLabel.setStyle("-fx-font-weight: bold")
    /*val toolbar = new ToolBar {
      layoutX = 540
      layoutY = 60
      content = List(possibleComLabel)
    }
    toolbar.border = Border.stroke(2)

    root.children += toolbar
*/

    val vbox = new VBox()
    vbox.layoutX = 540
    vbox.layoutY = 60
    vbox.border = Border.stroke(2)
    vbox.setSpacing(4.0)
    vbox.children += possibleComLabel
    vbox.setPadding(new Insets(10, 20, 10, 20)) // Padding top, right, bottom, left
    vbox.alignment = Pos.Center
    root.children += vbox

    // This method will place a candidate number into a rectangle, which the user clicks
    def placeCandidate(j: Int, row: Int, col: Int, board: Puzzleboard, candidate: String) =
      val amountOfSquaresHorizontal: Int = col / 3
      val amountOfSquaresVertical: Int = row / 3
      println(candidate)

      texts(j).setText("")
      val candidateNum = candidate.toIntOption

      def convertIndex(i: Int): Int =
        val m = (i / 9) / amountOfSquaresHorizontal // row of the 3x3 square
        val n = (i / 9) % amountOfSquaresHorizontal // col of the 3x3 square
        val topLeft3x3 = m * amountOfSquaresHorizontal * 3 * 3 + n * 3 // index of the tile in top left corner in 3x3square
        ((i % 9) / 3) * amountOfSquaresHorizontal * 3 + (i % 9) % 3 + topLeft3x3

      // Updating currentNumber of the tile in back-end
      val boardTiles = board.showTiles()
      val tileToPlaceCandidate = boardTiles(convertIndex(j))
      tileToPlaceCandidate.currentNumber = candidateNum

      // Placing the candidate to the rectangle
      val text = texts(j)
      text.setText(candidate)
      text.layoutX = tileToPlaceCandidate.xCoord + 18
      text.layoutY = tileToPlaceCandidate.yCoord + 26
      text.visible = true
      allListViews(j).visible = false



    def openListView(j: Int, row: Int, col: Int, board: Puzzleboard) =
      val amountOfSquaresHorizontal: Int = col / 3
      val amountOfSquaresVertical: Int = row / 3

      def convertIndex(i: Int): Int =
        val m = (i / 9) / amountOfSquaresHorizontal // row of the 3x3 square
        val n = (i / 9) % amountOfSquaresHorizontal // col of the 3x3 square
        val topLeft3x3 = m * amountOfSquaresHorizontal * 3 * 3 + n * 3 // index of the tile in top left corner in 3x3square
        ((i % 9) / 3) * amountOfSquaresHorizontal * 3 + (i % 9) % 3 + topLeft3x3

      // Displaying possible combinations in the gui.
      val sizeOfVbox: Int = vbox.getChildren.length
      vbox.getChildren.remove(1, sizeOfVbox)
      val combinations: Buffer[String] = board.showPossibleCombinationsInStr(convertIndex(j))
      val listOfLabels: List[Label] = combinations.toList.map( str => new Label(str) )
      // toolbar.getItems.removeAll()
      // listOfLabels.foreach( label => label.visible = true)
      // listOfLabels.foreach( label => label.layoutX = vbox.getLayoutX + 20 )
      listOfLabels.foreach( label => label.setTextFill(Color.Red) )
      listOfLabels.foreach( label => label.setFont(new Font(16)) )
      listOfLabels.foreach( label => vbox.children += label )
      // toolbar.getItems.addAll(listOfLabels.prepended(possibleComLabel): _*)



      val listView = allListViews(j)
      listView.toFront()
      // val text = new Text(listView.getSelectionModel.getSelectedItem)
      // text.append(listView.getSelectionModel.getSelectedItem)
      listView.getItems.remove(0, listView.getItems.length)
      // val candidates = board.getCandidates(convertIndex(j))
      val candidates = board.getCandidatesAfterSbaFilter(convertIndex(j))
      val subareaIndex: Int = board.showTiles()((convertIndex(j))).subareaIndex.get // TODO: Make sure it is defined.
      val amountOfFreeTiles: Int = board.showSubareas()(subareaIndex).showTiles().count( tile => tile.currentNumber.isEmpty )

      // Displays alertbox if we run out of candidates.
      if candidates.isEmpty && (amountOfFreeTiles != 0) then
        val alert = new Alert(AlertType.ERROR)
        alert.setTitle("Error")
        alert.setHeaderText(null)
        alert.setContentText("There are no candidates left! Please, consider removing some numbers from other squares or click 'Start again' button.")
        alert.showAndWait()
        println("Candidates is empty")
      else
        listView.getItems.add("")
        for k <- candidates.indices do
            listView.getItems.add(candidates(k).toString)
        end for

        listView.visible = true
        // val choice: String = listView.getSelectionModel.getSelectedItem
        listView.getSelectionModel.selectedItemProperty().addListener( e =>
          val candidate: String = listView.getSelectionModel.selectedItemProperty().get()
          if candidate != null then
            placeCandidate(j, row, col, board, candidate) )




    def tileHandler(j: Int) =
      tiles(j).setFill(Color.White)





    def createSubAreas(board: Puzzleboard, row: Int, col: Int) =
      val subareas: Vector[Subarea] = board.showSubareas()
      val tilesInBoard: Vector[Tile] = board.showTiles()
      val colors: Buffer[Color] = Buffer()
      val amountOfSquaresHorizontal: Int = col / 3
      val amountOfSquaresVertical: Int = row / 3


      // TODO: Use Greedy algorithm.
      // Creating the same amount of colors as subareas
      for i <- subareas.indices do
        colors += Color.rgb((16 * i + 128) % 255, (64 * i + 128) % 255, (128 * i + 128) % 255) // Color.rgb((200 + i * 4) % 255, (150 + i * 16) % 255, (150 + i) % 255)
      end for

      assert(tiles.length == tilesInBoard.length)
      def convertIndex(i: Int): Int =
        val m = (i / 9) / amountOfSquaresHorizontal // row of the 3x3 square
        val n = (i / 9) % amountOfSquaresHorizontal // col of the 3x3 square
        val topLeft3x3 = m * amountOfSquaresHorizontal * 3 * 3 + n * 3 // index of the tile in top left corner in 3x3square
        ((i % 9) / 3) * amountOfSquaresHorizontal * 3 + (i % 9) % 3 + topLeft3x3


      def cursorOut(j: Int) =
        val subIndex: Int = tilesInBoard(convertIndex(j)).subareaIndex.get
        tiles(j).setFill(colors(subIndex))
        allListViews(j).setOnMouseExited( e =>
          allListViews(j).visible = false )

      // Add color to every tile
      for j <- tiles.indices do
        try
          val subIndex: Int = tilesInBoard(convertIndex(j)).subareaIndex.get
          tiles(j).fill = colors(subIndex)

          // Checks if current tile has a target sum of the sub-area.
          if tilesInBoard(convertIndex(j)).targetSum.isDefined then

            val text = new Text(tilesInBoard(convertIndex(j)).targetSum.get.toString)
            // text.setFont(Font.font("Arial", FontWeight.BOLD, 14))
            text.setFill(Color.Black)
            text.setX(tilesInBoard(convertIndex(j)).xCoord)
            text.setY(tilesInBoard(convertIndex(j)).yCoord + 20)
            text.setFont(new Font(9))
            root.children += text
          end if

          initializeTextInTiles()

          // When hovering the color changes to white
          tiles(j).setOnMouseEntered( e =>
            tileHandler(j))
          // When the cursor leaves the tile, the color of the tile will be the same as before
          tiles(j).setOnMouseExited( e =>
            cursorOut(j))

          // When the user clicks on the tile, then the program should display a drop down menu of possible candidates
          tiles(j).setOnMouseClicked( e =>
            openListView(j, row: Int, col: Int, board: Puzzleboard) )

        catch
          case e => throw e
      end for


    // Menu
    val menuBar = new MenuBar
    val fileMenu = new Menu("File")

    val openPrevious = new Menu("Open previous")
    def updateOpenPrevious() =
      for i <- 0 until previousFiles.length do
        val currentMenuItem = new MenuItem(previousFiles(previousFiles.length - 1 - i))
        openPrevious.getItems.add(currentMenuItem)
      end for

    val newFileItem = new MenuItem("New file")
    newFileItem.onAction = (event) =>

      try
        println("New file -button in the menubar is clicked")
        val fileChooser = new FileChooser
        fileChooser.setTitle("Open new file")
        fileChooser.getExtensionFilters.addAll(new FileChooser.ExtensionFilter("TEXT", "*.txt"))
        val file = fileChooser.showOpenDialog(stage)

        if file != null then
          val lines = FileReader.readFile(file.toString) // returns all lines in the given file
          val boardWithSize = FileReader.readFilePuzzleBoardCfg(lines) // Returns (board, row, column)
          val board = boardWithSize._1
          puzzleboard = Some(board)
          create3x3Squares(boardWithSize._2, boardWithSize._3, board)
          initializeListViews(board, boardWithSize._2, boardWithSize._3)
          createSubAreas(board, boardWithSize._2, boardWithSize._3)
          previousFiles += file.toString
          updateOpenPrevious()
          for i <- board.showTiles().indices do
            val tile = board.showTiles()(i)
            println(s"column: ${tile.getColumn}, row: ${tile.getRow}, targetSum: ${tile.targetSum}, square: ${tile.getSquare}.")
        // C:\Users\imran\IdeaProjects\Killer_Sudoku\src\testingData

        else
          val alert = new Alert(AlertType.ERROR)
          alert.setTitle("Error")
          alert.setHeaderText(null)
          alert.setContentText("File not found error! Please, make sure that you selected the correct file.")
          alert.showAndWait()
        end if

      catch
        case e => throw e



      val downloadFile = new TextInputDialog("path")
        downloadFile.initOwner(stage)
        downloadFile.title = "New file"
        downloadFile.headerText = ""
        downloadFile.contentText = "Please, provide filepath here: "
        downloadFile.getDialogPane.setMinSize(400, 350)

      val result = downloadFile.showAndWait() // Result from the user input
      result match
        case filepath      => println("" + filepath)
        // case Optional(filepath) => println("" + filepath)
        // case None           => println("None")
        // case None           => println("Filepath was not provided")



    /*openPreviousItem.onAction = (event) =>
        try
          println("Open previous -button in the menubar is clicked")
          val recentFilesMenu = new Menu("Open previous")
          previousFiles.foreach( path =>
            val menuItem = new MenuItem(path)
            menuItem.onAction = (event) =>
              println("Path clicked")
            recentFilesMenu.getItems.add(menuItem)
          )
*/

    val saveItem = new MenuItem("Save")
      saveItem.onAction = (event) => println("Save-button in the menubar is clicked")
    val saveAsItem = new MenuItem("Save as")
      saveAsItem.onAction = (event) => println("Save as -button in the menubar is clicked")

    fileMenu.items = List(newFileItem, SeparatorMenuItem(), openPrevious, SeparatorMenuItem(), saveItem, SeparatorMenuItem(), saveAsItem)
    menuBar.menus = List(fileMenu)
    root.children += menuBar



    def showAlertStartAgain() =
      val alert = new Alert(AlertType.CONFIRMATION)
      alert.setTitle("Confirmation")
      alert.setHeaderText(null)
      alert.setContentText("Are you sure you want to start over?")

      val yesButtonType = ButtonType.YES
      val noButtonType = ButtonType.NO
      alert.getButtonTypes.setAll(yesButtonType, noButtonType)

      val result = alert.showAndWait()

      def deleteText() =
        texts.foreach( text => text.setText("") )

      def deletePossibleCombinations() =
        val sizeOfBox: Int = vbox.getChildren.length
        vbox.getChildren.remove(1, sizeOfBox)

      result.ifPresent( e =>
          if (e.getText == "Yes") && (puzzleboard.isDefined) then
            puzzleboard.get.showTiles().foreach( tile => tile.currentNumber = None )
            deleteText()
            deletePossibleCombinations()
            println("Yes button is clicked")
          else
            println("No button clicked or dialog closed") )

    // Start Again -button
    val startAgainButton = Button("Start Again")
     startAgainButton.onAction = (event) => showAlertStartAgain()
     startAgainButton.layoutX = 450
     startAgainButton.layoutY = 0
     startAgainButton.setMinSize(80, 35)
     startAgainButton.border = Border.stroke(2)

     root.children += startAgainButton



/*
    // Undo step -button
    val input = new FileInputStream("src/images/left_arrow_circle_shape.png")
    val image = new Image(input)
    val imageViewUndo = new ImageView(image)

    val undoButton = Button(null, imageViewUndo)
      undoButton.scaleX = 0.5
      undoButton.scaleY = 0.5
      undoButton.layoutX = 40
      undoButton.layoutY = 450
      undoButton.setShape(new Circle(70))
      undoButton.setPadding(Insets.Empty)
    root.children += undoButton

    // Redo step -button
    val imageViewRedo = new ImageView(image)
      imageViewRedo.scaleX = -imageViewRedo.getScaleX

    val redoButton = Button(null, imageViewRedo)
      redoButton.scaleX = 0.5
      redoButton.scaleY = 0.5
      redoButton.layoutX = 320
      redoButton.layoutY = 450
      redoButton.setShape(new Circle(70))
      redoButton.setPadding(Insets.Empty)
    root.children += redoButton

*/


end Main
