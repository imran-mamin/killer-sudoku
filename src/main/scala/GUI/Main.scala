package GUI


import javafx.geometry.{HPos, VPos}
import javafx.scene.control.{ContextMenu, ListView, TextField}
import javafx.scene.layout.GridPane
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.{Background, Border, Pane, StackPane, TilePane}
import scalafx.scene.control.{Button, Label, Menu, MenuBar, MenuItem, SeparatorMenuItem}
import scalafx.Includes.*
import scalafx.geometry.Pos
import scalafx.scene.canvas.*
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.text.Text
import scalafx.geometry.Insets
import scalafx.scene.Group
import scalafx.scene.layout.Region

import scala.collection.mutable.Buffer
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.control.{TextInputDialog, ToolBar}
import javafx.scene.shape.{Circle, Line}
import javafx.scene.text.TextAlignment
import javafx.stage.FileChooser
import sudoku.*

import java.awt.TextArea
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


    val mainScene = Scene(parent = root)
    stage.scene = mainScene

    val tiles: Buffer[Rectangle] = Buffer()

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

/*
    def initializeTilesCoordinates(i: Int, j: Int, board: Puzzleboard, squaresHorizontal: Int, squaresVertical: Int): Unit =
      val startIndex = i * squaresHorizontal * 9 + j * 9
      val boardTiles = board.showTiles()
      var multiplier = 0
      println(tiles.length)
      for k <- startIndex until tiles.length do
        // Set coordinate of top left corner of the tile

        // Multiplied by three because one square 3x3 has three tiles in a row.
        val squareX = gridWith3x3Squares.getLayoutX + j * 3 * tiles(0).getWidth
        val squareY = gridWith3x3Squares.getLayoutY + i * 3 * tiles(0).getHeight
        val tile = k % 3
/*
        boardTiles(k).xCoord = tile * tiles(0).getWidth + squareX
        boardTiles(k).yCoord = multiplier * tiles(0).getHeight + squareY
        if tile == 0 then
          multiplier += 1*/
        println(s"SquareX: ${squareX}, SquareY: ${squareY}")
      end for
*/

    def setCoordinates(board: Puzzleboard, row: Int, col: Int) =
      val boardTiles = board.showTiles()

      for i <- 0 until row do
        for j <- 0 until col do
          boardTiles((i * col) + j).xCoord = gridWith3x3Squares.getLayoutX + j * tiles(0).getWidth
          boardTiles((i * col) + j).yCoord = gridWith3x3Squares.getLayoutY + i * tiles(0).getHeight
          boardTiles((i * col) + j).edgeSize = tiles(0).getWidth
        end for
      end for


    def rearrangeTilesAsInBackEnd(row: Int, col: Int) =
      var orderedTiles = Array.fill(tiles.length)
      val amountOfSquaresHorizontal: Int = col / 3
      val amountOfSquaresVertical: Int = row / 3

      val squares: Buffer[Buffer[Rectangle]] = tiles.grouped(9).toBuffer

      for i <- squares.indices do
        for j <- squares(i).indices do
          val currentSquare = squares(i)
          currentSquare(j)
        end for
      end for
      ???

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

    def tileHandler(j: Int) =
      tiles(j).setFill(Color.White)

    def createSubAreas(board: Puzzleboard, row: Int, col: Int) =
      val subareas: Vector[Subarea] = board.showSubareas()
      val tilesInBoard: Vector[Tile] = board.showTiles()
      val colors: Buffer[Color] = Buffer()
      val amountOfSquaresHorizontal: Int = col / 3
      val amountOfSquaresVertical: Int = row / 3


      // Creating the same amount of colors as subareas
      for i <- subareas.indices do
        colors += Color.rgb((16 * i + 128) % 255, (32 * i + 128) % 255, (60 * i + 128) % 255)
      end for

      assert(tiles.length == tilesInBoard.length)

      def convertIndex(i: Int): Int =
        val m = (i / 9) / amountOfSquaresHorizontal
        val n = (i / 9) % amountOfSquaresHorizontal
        val topLeft3x3 = m * amountOfSquaresHorizontal * 3 + n * 3
        ((i % 9) / 3) * amountOfSquaresHorizontal * 3 + (i % 9) % 3 + topLeft3x3

      // Add color to every tile
      for j <- tiles.indices do
        try
          val subIndex: Int = tilesInBoard(convertIndex(j)).subareaIndex.get
          tiles(j).fill = colors(subIndex)
          // When hovering the color changes to white
          tiles(j).setOnMouseEntered( e =>
            tileHandler(j))
          // When the cursor leaves the tile, the color of the tile will be the same as before
          tiles(j).setOnMouseExited( e => tiles(j).setFill(colors(subIndex)))

          // Checks if current tile has a target sum of the sub-area.
          if tilesInBoard(j).targetSum.isDefined then

            val text = new Text(tilesInBoard(j).targetSum.get.toString)
            // text.setFont(Font.font("Arial", FontWeight.BOLD, 14))
            text.setFill(Color.Yellow)
            text.setX(tiles(j).getX() + 10)
            text.setY(tiles(j).getY() + 20)

          end if
        catch
          case e => throw e
      end for


    // Menu
    val menuBar = new MenuBar
    val fileMenu = new Menu("File")
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
          create3x3Squares(boardWithSize._2, boardWithSize._3, board)


          createSubAreas(board, boardWithSize._2, boardWithSize._3)

        /*
          tiles(80).setOnMouseClicked(
            mouseEvent => {
              /*val listView = new ListView[String]()
              listView.getItems.add("1")
              listView.getItems.add("2")
              listView.layoutX = 100
              listView.layoutY = 100
              listView.setMaxSize(100, 100)
              val text = new Text(listView.getSelectionModel.getSelectedItem)
              // text.append(listView.getSelectionModel.getSelectedItem)
              // tiles(80).accessibleText = listView.getSelectionModel.getSelectedItem
              root.children += listView
              // listView.selectionModel().selectedItemProperty().addListener((_, _, newValue) => {
              // tiles(80).accessibleText = newValue
              // })*/

              val popupMenu = new ContextMenu()
              val one = new MenuItem("1")
              val two = new MenuItem("2")
              popupMenu.getItems.add(one)
              popupMenu.getItems.add(two)
              // root.children += popupMenu
            }) */

// C:\Users\imran\IdeaProjects\Killer_Sudoku\src\testingData

        else
          assert(false)
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


    val openPreviousItem = new MenuItem("Open previous")
      openPreviousItem.onAction = (event) => println("Open previous -button in the menubar is clicked")
    val saveItem = new MenuItem("Save")
      saveItem.onAction = (event) => println("Save-button in the menubar is clicked")
    val saveAsItem = new MenuItem("Save as")
      saveAsItem.onAction = (event) => println("Save as -button in the menubar is clicked")

    fileMenu.items = List(newFileItem, SeparatorMenuItem(), openPreviousItem, SeparatorMenuItem(), saveItem, SeparatorMenuItem(), saveAsItem)
    menuBar.menus = List(fileMenu)
    root.children += menuBar



    // Start Again -button
    val startAgainButton = Button("Start Again")
     startAgainButton.onAction = (event) => println("Clicked Start again -button")
     startAgainButton.layoutX = 450
     startAgainButton.layoutY = 0
     startAgainButton.setMinSize(80, 35)
     startAgainButton.border = Border.stroke(2)

     root.children += startAgainButton



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


    // Possible combinations of the tiles in subarea
    val toolbar = new ToolBar {
      layoutX = 540
      layoutY = 60
      content = List(
        new Label("Possible combinations")
      )
    }
    toolbar.border = Border.stroke(2)

    root.children += toolbar
end Main
