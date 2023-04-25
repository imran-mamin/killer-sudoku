package sudoku.GUI

import scalafx.scene.input.{KeyCode, KeyEvent}
import javafx.beans.property.ReadOnlyObjectProperty
import javafx.geometry.{HPos, VPos}
import javafx.scene.{Node, control, shape}
import javafx.scene.control.{Alert, ButtonType, ChoiceDialog, ContextMenu, ListCell, ListView, MenuButton, TextField}
import javafx.scene.control.Alert.AlertType
import javafx.scene.layout.{GridPane, VBox}
import scalafx.application.JFXApp3
import scalafx.scene.{Group, Scene, shape}
import scalafx.scene.layout.{Background, Border, Pane, StackPane, TilePane}
import scalafx.scene.control.{Button, Label, Menu, MenuBar, MenuItem, SeparatorMenuItem}
import scalafx.Includes.*
import scalafx.geometry.Pos
import scalafx.scene.canvas.*
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight, Text}
import javafx.geometry.Insets
import javafx.scene
import scalafx.scene.layout.Region
import scala.collection.mutable.Buffer
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.control.{TextInputDialog, ToolBar}
import javafx.scene.shape.{Circle, Line, Shape}
import javafx.scene.text.TextAlignment
import javafx.stage.{FileChooser, WindowEvent}
import sudoku.*
import sudoku.GUI.CustomListCell
import javafx.stage.FileChooser.ExtensionFilter
import java.awt.{Cursor, Shape, TextArea}
import java.io.FileInputStream
import java.io.File
import scala.language.postfixOps
import scalafx.scene.layout.BorderStroke
import scalafx.scene.layout.BorderStrokeStyle
import scalafx.scene.layout.BorderWidths



object Main extends JFXApp3:


  def start(): Unit =

    // This method Displays an alert in the gui window.
    def throwAlert(atype: AlertType, title: String, message: String) =
      val alert = new Alert(atype)
          alert.setTitle(title)
          alert.setHeaderText(null)
          alert.setContentText(message)
          alert.showAndWait()
    end throwAlert


    stage = new JFXApp3.PrimaryStage:
      title = "Killer-Sudoku"
      width = 800
      height = 600


    val root = Pane()
    val gridWith3x3Squares = GridPane()
    gridWith3x3Squares.layoutY = 80
    gridWith3x3Squares.layoutX = 80

    var unsavedChanges: Boolean = false // Flag to track unsaved changes in the file
    var puzzleboardIsUpdated: Boolean = false // Flag to check whether file opening was successful or not
                                              // Is used later, for disabling save items.
    var puzzleboard: Option[Puzzleboard] = None
    var colNSize: Option[Int] = None
    var rowNSize: Option[Int] = None
    val previousFiles = Buffer[String]()
    val mainScene = Scene(parent = root)
    stage.scene = mainScene


    /** Convert tile index in gui to back-end */
    def convertIndex(i: Int): Int =
      val amountOfSquaresHorizontal: Int = colNSize.get / 3
      val m = (i / 9) / amountOfSquaresHorizontal // row of the 3x3 square
      val n = (i / 9) % amountOfSquaresHorizontal // col of the 3x3 square
      val topLeft3x3 = m * amountOfSquaresHorizontal * 3 * 3 + n * 3 // index of the tile in top left corner in 3x3square
      ((i % 9) / 3) * amountOfSquaresHorizontal * 3 + (i % 9) % 3 + topLeft3x3
    end convertIndex


    val tiles: Buffer[Rectangle] = Buffer()

    // This information will be used later when calculating coordinates of tiles.
    var gridWith9TilesInsets: Insets = Insets.EMPTY

    // To avoid "Advanced language feature: postfix operator notation" warning
    def updateUnsavedChanges(b: Boolean): Unit =
      unsavedChanges = b


    def updateTitle(): Unit =
      var title: String = stage.getTitle

      // Add asterisk, if there are unsaved changes in the file.
      if unsavedChanges && !stage.getTitle.contains("*") then
        title = title + " - *"
      end if

      // Remove asterisk, if there are not unsaved changes in the file.
      if !unsavedChanges && stage.getTitle.contains("*") then
        title = title.dropRight(4) // Four because " - *" contains four chars.
      end if
      stage.title = title


    def create9Tiles(): GridPane =
      val gridWith9Tiles = GridPane()

      for i <- 0 until 3 do
        for j <- 0 until 3 do
          val rectangle = new Rectangle:
            width = 40
            height = 40
          rectangle.setStroke(Color.Black)
          rectangle.setStrokeWidth(0.5)
          tiles += rectangle
          gridWith9Tiles.add(rectangle, j + 1, i + 1)
      gridWith9Tiles.border = Border.stroke(Color.Black)
      gridWith9TilesInsets = gridWith9Tiles.getBorder.getInsets
      gridWith9Tiles


    val characters: List[Char] = ('a' to 'z').toList

    // Show a,b,c,... and 1,2,3,... coordinates of puzzleboard
    def showNumAndCharCoordinates() =
      val boardTiles: Vector[Tile] = puzzleboard.get.getTiles
      for i <- tiles.indices do
        if i / colNSize.get == 0 then
           val col = new Label:
              text = characters(i).toString
           col.layoutX = boardTiles(i).xCoord + tiles.head.getWidth / 2.0
           col.layoutY = gridWith3x3Squares.getLayoutY - 0.25 * gridWith3x3Squares.getLayoutY
           root.children += col
        end if

        if i % rowNSize.get == 0 then
          // Displaying the y-coordinates in the GUI window
          val rowText = new Label:
            text = "" + (i / rowNSize.get + 1) // Tells row of tiles
          // - 1/4 is to display text on top of tiles at the center.
          rowText.layoutX = gridWith3x3Squares.getLayoutX - 0.25 * gridWith3x3Squares.getLayoutX
          // Starting point plus amount of tiles multiplied by their height
          rowText.layoutY = boardTiles(i).yCoord + tiles.head.getHeight / 3.0
          root.children += rowText
        end if
      end for


    // Set coordinates of the tile's top left corner and store it in the back-end
    def setTopLeftXYTileCoordinates() =
      val boardTiles = puzzleboard.get.getTiles

      for i <- 0 until rowNSize.get do
        for j <- 0 until colNSize.get do
          val borderLeftOf9x9Square: Double = gridWith3x3Squares.getBorder.getInsets.getLeft
          val borderLeftOf3x3Square: Double = gridWith9TilesInsets.getLeft
          val tileBorder: Double = tiles.head.getStrokeWidth // Border of rectangle object
          // println(gridWith3x3Squares.getChildren.get(2).localToScene(i, j))
          // X-coordinate is layoutX of 9x9 Square + tilesWidth * amountOfTiles before this one +
          // the border of 9x9 square + borders of 3x3 squares + single tile border.
          boardTiles((i * colNSize.get) + j).xCoord = gridWith3x3Squares.getLayoutX + j * tiles.head.getWidth +
            borderLeftOf9x9Square + ((j / 3) * 2 + 1) * borderLeftOf3x3Square + (2 * j + 1) * tileBorder

          val borderTopOf9x9Square: Double = gridWith3x3Squares.getBorder.getInsets.getTop
          val borderTopOf3x3Square: Double = gridWith9TilesInsets.getTop
          boardTiles((i * colNSize.get) + j).yCoord = gridWith3x3Squares.getLayoutY + i * tiles.head.getHeight +
            borderTopOf9x9Square + ((i / 3) * 2 + 1) * borderTopOf3x3Square + (2 * i + 1) * tileBorder
        end for
      end for


    def create3x3Squares() =
      val amountOfSquaresHorizontal: Int = colNSize.get / 3
      val amountOfSquaresVertical: Int = rowNSize.get / 3

      for i <- 0 until amountOfSquaresVertical do  // rows (y)
        for j <- 0 until amountOfSquaresHorizontal do  // columns (x)
          gridWith3x3Squares.add(create9Tiles(), j, i)
        end for
      end for
      gridWith3x3Squares.border = Border.stroke(Color.Black) // Add border to puzzleboard.
      setTopLeftXYTileCoordinates() // Set top left corner's coordinates of rectangle object.
      showNumAndCharCoordinates()   // Show coordinates for user.


    root.children += gridWith3x3Squares // Add gridWith3x3Squares to root.


    val candidateLists: Buffer[ListView[String]] = Buffer()

    def initializeCandidateLists(): Unit =
      val amountOfSquaresHorizontal: Int = colNSize.get / 3
      val amountOfSquaresVertical: Int = rowNSize.get / 3
      val boardTiles = puzzleboard.get.getTiles

      for j <- tiles.indices do
        val currentListView = new ListView[String]()
        currentListView.setMaxSize(100, 100)
        currentListView.visible = false
        currentListView.setCellFactory(_ => new CustomListCell(tiles.toVector, puzzleboard.get, colNSize.get))
        candidateLists += currentListView
        root.children += currentListView
        currentListView.getItems.add("")
        val candidates = puzzleboard.get.getCandidatesAfterSbaFilter(convertIndex(j))

        for k <- candidates.indices do
          currentListView.getItems.add(candidates(k).toString)
        end for

        val xCoord = boardTiles(convertIndex(j)).xCoord
        val yCoord = boardTiles(convertIndex(j)).yCoord
        currentListView.layoutX = xCoord - tiles.head.getWidth / 2.0
        currentListView.layoutY = yCoord + tiles.head.getHeight
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
    possibleComLabel.setFont(new Font(18))
    possibleComLabel.setStyle("-fx-font-weight: bold")


    val vbox = new VBox()
    vbox.layoutX = 540
    vbox.layoutY = 60
    vbox.border = Border.stroke(2)
    vbox.setSpacing(4.0)
    vbox.children += possibleComLabel
    vbox.setPadding(new Insets(10, 20, 10, 20)) // Padding top, right, bottom, left
    vbox.alignment = Pos.Center
    root.children += vbox


    var numOffsetX = 15
    var numOffsetY = 26

    // Place a candidate number into a rectangle, which user clicks
    def placeCandidate(j: Int, candidate: String) =
      val amountOfSquaresHorizontal: Int = colNSize.get / 3
      val amountOfSquaresVertical: Int = rowNSize.get / 3
      texts(j).setText("")
      val candidateNum = candidate.toIntOption

      // Update currentNumber of the tile in back-end
      val boardTiles = puzzleboard.get.getTiles
      val tileToPlaceCandidate = boardTiles(convertIndex(j))
      tileToPlaceCandidate.currentNumber = candidateNum

      // Place the candidate to the rectangle
      val text = texts(j)
      text.setText(candidate)
      text.layoutX = tileToPlaceCandidate.xCoord + numOffsetX
      text.layoutY = tileToPlaceCandidate.yCoord + numOffsetY
      text.setFont(Font.font("Arial", FontWeight.Bold, 14))
      text.visible = true
      candidateLists(j).visible = false
      updateUnsavedChanges(true)
      updateTitle()


    def openCandidatesListView(j: Int) =
      val amountOfSquaresHorizontal: Int = colNSize.get / 3
      val amountOfSquaresVertical: Int = rowNSize.get / 3

      // Show possible combinations
      showPossibleCombs(j)

      // Display a listView object
      val listView = candidateLists(j)
      listView.setOnMouseEntered( e => showPossibleCombs(j) ) // Show combinations also, when the cursor is in ListView.
      listView.toFront()
      listView.getItems.remove(0, listView.getItems.length)
      val candidates = puzzleboard.get.getCandidatesAfterSbaFilter(convertIndex(j))
      val subareaIndex: Int = puzzleboard.get.getTiles((convertIndex(j))).subareaIndex.get
      val amountOfFreeTiles: Int = puzzleboard.get.getSubareas(subareaIndex).getTiles.count(tile => tile.currentNumber.isEmpty )

      // Display alertbox if we run out of candidates and there are no placed num in the tile.
      if candidates.isEmpty && (amountOfFreeTiles != 0) && (texts(j).getText == "") then
        val alertMessage: String = "There are no candidates left! Please, consider removing some numbers from other squares or click 'Start again' button."
        throwAlert(AlertType.ERROR, "Error", alertMessage)
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
            placeCandidate(j, candidate) )


    def showPossibleCombs(j: Int): Unit =
      // Displaying possible combinations in the gui.
      val sizeOfVbox: Int = vbox.getChildren.length
      vbox.getChildren.remove(1, sizeOfVbox)
      val combinations: Buffer[String] = puzzleboard.get.getCombinationsInStr(convertIndex(j))
      val listOfLabels: List[Label] = combinations.toList.map( str => new Label(str) )
      listOfLabels.foreach( label => label.setTextFill(Color.Red) )
      listOfLabels.foreach( label => label.setFont(new Font(16)) )
      listOfLabels.foreach( label => vbox.children += label )


    def tileHandler(j: Int) =
      tiles(j).setFill(Color.White)


    var cursorInRectOrListView: Boolean = false

    def showSubareas() =
      val subareas: Vector[Subarea] = puzzleboard.get.getSubareas
      val tilesInBoard: Vector[Tile] = puzzleboard.get.getTiles
      assert(tiles.length == tilesInBoard.length)

      def handleCursorOut(j: Int) =
        val subIndex: Int = tilesInBoard(convertIndex(j)).subareaIndex.get
        val subareaColorInRGB = convertHSBtoRGB(subareas(subIndex).color.get)
        tiles(j).setFill(Color.rgb(subareaColorInRGB._1, subareaColorInRGB._2, subareaColorInRGB._3))
        candidateLists(j).setOnMouseExited( e =>
          candidateLists(j).visible = false )

      // Add color to every tile
      for j <- tiles.indices do
        try
          val subIndex: Int = tilesInBoard(convertIndex(j)).subareaIndex.get
          if subareas(subIndex).color.isDefined then
            val rgbCodes = convertHSBtoRGB(subareas(subIndex).color.get)
            tiles(j).fill = Color.rgb(rgbCodes._1, rgbCodes._2, rgbCodes._3) // colors(subIndex)
          else
            assert(false)
          // Checks if current tile has a target sum of the sub-area.
          if tilesInBoard(convertIndex(j)).targetSum.isDefined then
            val text = new Text(tilesInBoard(convertIndex(j)).targetSum.get.toString)
            text.setFill(Color.Black)
            text.setX(tilesInBoard(convertIndex(j)).xCoord + 1)
            text.setY(tilesInBoard(convertIndex(j)).yCoord + 9)
            text.setFont(new Font(9))
            text.setMouseTransparent(true) // Gives an ability for a user to click on the text and it will still open ListView-object.
            root.children += text
          end if
          initializeTextInTiles()

          // When hovering, the color changes to white
          tiles(j).setOnMouseEntered( e =>
            showPossibleCombs(j)
            tileHandler(j)
          )

          // When the cursor leaves the tile, the color of the tile will be the same as before
          tiles(j).setOnMouseExited( e =>
            handleCursorOut(j)
          )

          // When the user clicks on the tile, then the program should display a drop down menu of possible candidates
          tiles(j).setOnMouseClicked( e =>
            tileHandler(j)
            candidateLists.foreach( listview => listview.visible = false )
            openCandidatesListView(j)
          )
          texts(j).setMouseTransparent(true) // Cursor should ignore text placed in tile.
        catch
          case e => throw e
      end for


    // Remove possible combinations, when cursor goes out of the GridPane object.
    gridWith3x3Squares.setOnMouseExited( e => removeCombinations() )


    def showPuzzleboardUserNums(): Unit =
      val tilesInBoard: Vector[Tile] = puzzleboard.get.getTiles
      val amountOfSquaresHorizontal: Int = colNSize.get / 3
      val amountOfSquaresVertical: Int = rowNSize.get / 3

      for i <- tiles.indices do
        if tilesInBoard(convertIndex(i)).currentNumber.isDefined then
          val tileWithNum = tilesInBoard(convertIndex(i))
          val currNum: Int = tileWithNum.currentNumber.get
          texts(i).setText(currNum.toString)
          texts(i).layoutX = tileWithNum.xCoord + numOffsetX
          texts(i).layoutY = tileWithNum.yCoord + numOffsetY
          texts(i).setFont(Font.font("Arial", FontWeight.Bold, 14))
          texts(i).visible = true
        end if
      end for


    def initCanvas(): Unit =
      gridWith3x3Squares.getChildren.remove(0, gridWith3x3Squares.getChildren.length)
      gridWith9TilesInsets = Insets.EMPTY
      candidateLists.remove(0, candidateLists.length)
      tiles.remove(0, tiles.length)

      // Will remove all the Text or Label nodes from the root (Pane()).
      root.getChildren.removeIf((node: Node) => {
        node.isInstanceOf[scalafx.scene.text.Text] || node.isInstanceOf[control.Label] || node.isInstanceOf[javafx.scene.text.Text]
      })


    def removeTextObjects(): Unit =
      texts.remove(0, texts.length)


    def removeCombinations() =
      val sizeOfBox: Int = vbox.getChildren.length
      vbox.getChildren.remove(1, sizeOfBox)


    // Menu
    val menuBar = new MenuBar
    val fileMenu = new Menu("File")

    var fileNameOfSavedFile: Option[String] = None // Will contain the name of a file that have already been saved.
    var parentOfSavedFile: Option[File] = None // Will contains the parent directory of the saved file.


    def openFileChooserToSaveFile: Unit =
      val fileChooser = new FileChooser()
        fileChooser.setTitle("Save File")

        // set the extension filter to .txt
        fileChooser.getExtensionFilters.add(new ExtensionFilter("Text Files", "*.txt"))

        // Set the initial directory
        val initialDir = new File(System.getProperty("user.home"))
        fileChooser.setInitialDirectory(initialDir)

        // Set initial name of the file
        val initialFileName = "Attempt.txt"// s"myfile_${openPrevious.getItems.length}.txt"
        fileChooser.setInitialFileName(initialFileName)

        // Show save dialog and get the selected file
        val selectedFile = fileChooser.showSaveDialog(null)
        if (selectedFile != null) then
          val fileName = selectedFile.getName
          val parentDir: File = selectedFile.getParentFile
          updateUnsavedChanges(false)
          updateTitle()
          println(parentDir)

          if puzzleboard.isDefined then
            FWriter.writeFile(fileName, parentDir, puzzleboard.get, rowNSize.get, colNSize.get)
            fileNameOfSavedFile = Some(fileName)
            parentOfSavedFile = Some(parentDir)
          else
            val alertMessage: String = "File saving was unsuccessful! Please, make sure that you downloaded a board first."
            throwAlert(AlertType.WARNING, "Cannot save the file!", alertMessage)


    val saveItem = new MenuItem("Save")
      saveItem.disable = true // In the beginning should be disabled
      saveItem.onAction = (event) =>
        println("Save-button in the menubar is clicked")
        if puzzleboardIsUpdated then
          saveItem.disable = false
          if fileNameOfSavedFile.isDefined && parentOfSavedFile.isDefined then
            FWriter.writeFile(fileNameOfSavedFile.get, parentOfSavedFile.get, puzzleboard.get, rowNSize.get, colNSize.get)
            updateUnsavedChanges(false) // Delete asterisk
            updateTitle()
          else
            openFileChooserToSaveFile
        else
          saveItem.disable = true


    val saveAsItem = new MenuItem("Save as")
      saveAsItem.disable = true  // In the beginning should be disabled
      saveAsItem.onAction = (event) =>
        if puzzleboardIsUpdated then
          saveAsItem.disable = false
          println("Save as -button in the menubar is clicked")
          openFileChooserToSaveFile // Opens a fileChooser, where the user can select, where she/he wants to save the file.
        else
          saveAsItem.disable = true


    val newFileItem = new MenuItem("New file")
    newFileItem.onAction = (event) =>

      try
        println("New file -button in the menubar is clicked")
        val fileChooser = new FileChooser
        fileChooser.setTitle("Open new file")
        fileChooser.getExtensionFilters.addAll(new FileChooser.ExtensionFilter("TEXT", "*.txt"))
        val file = fileChooser.showOpenDialog(stage)

        if file != null then
          // Remove previous board from the gui window.
          removeTextObjects()
          removeCombinations()
          initCanvas()

          // Create new board according to the provided file.
          val lines = FileReader.readFile(file.toString) // return all lines in the given file
          val boardWithSize = FileReader.readFilePuzzleBoardCfg(lines) // Return (board, row, column)
          val board = boardWithSize._1
          puzzleboard = Some(board)
          rowNSize = Some(boardWithSize._2)
          colNSize = Some(boardWithSize._3)
          // Delete name of previous file.
          stage.title = "Killer-Sudoku"
          stage.title = stage.getTitle + " - " + boardWithSize._4
          // Create sudoku board
          create3x3Squares()
          // Create listView object for every tile.
          initializeCandidateLists()
          // Color sub-areas with different colors.
          showSubareas()
          showPuzzleboardUserNums()
          previousFiles += file.toString

          // Remove disabling of save items
          puzzleboardIsUpdated = true
          saveItem.disable = false
          saveAsItem.disable = false
      catch
        case e: java.io.IOException =>
          puzzleboardIsUpdated = false // Cannot save file if we cannot open it
          saveItem.disable = true      // Disable saving ability
          saveAsItem.disable = true
          val alertMessage: String = "The IOException was occuried while trying to open the file."
          throwAlert(AlertType.ERROR, "Error", alertMessage)

        case e: java.io.FileNotFoundException =>
          puzzleboardIsUpdated = false
          saveItem.disable = true
          saveAsItem.disable = true
          val alertMessage: String = "File not found error! Please, make sure that you selected the correct file."
          throwAlert(AlertType.ERROR, "Error", alertMessage)

        case e: java.util.NoSuchElementException =>
          puzzleboardIsUpdated = false
          saveItem.disable = true
          saveAsItem.disable = true
          val alertMessage: String = "Cannot create the board, because the amount of rows and columns should be divisible by three." +
            "Please, make sure that you specified the proper amount of tiles in the file."
          throwAlert(AlertType.ERROR, "Error", alertMessage)

        case e: AssertionError =>
          puzzleboardIsUpdated = false
          saveItem.disable = true
          saveAsItem.disable = true
          val alertWithAssert: String = e.getMessage
          val alertMessage: String = alertWithAssert.split("assertion failed:").apply(1).trim
          throwAlert(AlertType.ERROR, "Error", alertMessage)


    fileMenu.items = List(newFileItem, SeparatorMenuItem(), saveItem, SeparatorMenuItem(), saveAsItem)
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

      result.ifPresent( e =>
          if (e.getText == "Yes") && (puzzleboard.isDefined) then
            puzzleboard.get.getTiles.foreach( tile => tile.currentNumber = None )
            deleteText()
            removeCombinations()
            updateUnsavedChanges(true)
            updateTitle()
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


    // The warning alert will appear, if there are unsaved changes and the user clicks on exit
    // button in the gui.
    stage.onCloseRequest = (event: WindowEvent) =>
      if unsavedChanges then
        val alertClose = new Alert(Alert.AlertType.WARNING)
        alertClose.contentText = "There are unsaved changes. Would you like to save changes?"
        alertClose.title = "Unsaved Changes"
        alertClose.buttonTypes = Seq(ButtonType.YES, ButtonType.NO, ButtonType.CANCEL)
        val result = Option(alertClose.showAndWait().orElse(null))

        result match
          case Some(ButtonType.YES) =>
            openFileChooserToSaveFile
            event.consume()
          case Some(ButtonType.NO) =>
            // Discard changes
            sys.exit(0)
          case _ =>
            // Cancel closing the window
            event.consume()

end Main
