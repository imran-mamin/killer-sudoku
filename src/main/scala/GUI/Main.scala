package GUI


import javafx.geometry.{HPos, VPos}
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

import scala.collection.mutable.Buffer
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.control.{TextInputDialog, ToolBar}
import javafx.scene.shape.{Circle, Line}
import javafx.scene.text.TextAlignment
import javafx.stage.FileChooser
import sudoku.FileReader

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

    val characters: List[Char] = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')

    def create9Tiles(): GridPane =
      val gridWith9Tiles = GridPane()

      for i <- 0 until 3 do
        for j <- 0 until 3 do
          val rectangle = new Rectangle:
            width = 40
            height = 40
            fill = Color.LightGrey
          rectangle.setStroke(Color.Black)
          rectangle.setStrokeWidth(0.5)
          // When hovering the color changes to white
          rectangle.setOnMouseEntered( e => rectangle.setFill(Color.White))
          // When the cursor leaves the tile, the color of the tile will be the same as before
          rectangle.setOnMouseExited( e => rectangle.setFill(Color.LightGrey))
          tiles += rectangle
          gridWith9Tiles.add(rectangle, j + 1, i + 1)
      gridWith9Tiles.border = Border.stroke(Color.Black)
      gridWith9Tiles




    def create3x3Squares(row: Int, col: Int) =
      val amountOfSquaresHorizontal: Int = col / 3
      val amountOfSquaresVertical: Int = row / 3

      for i <- 0 until amountOfSquaresVertical do  // rows (y)
        for j <- 0 until amountOfSquaresHorizontal do  // columns (x)
          gridWith3x3Squares.add(create9Tiles(), j, i)
          gridWith3x3Squares.border = Border.stroke(Color.Black)
        end for
      end for
      root.children += gridWith3x3Squares
    /*
    def createGUIBoard(row: Int, col: Int) =
      // Adding tiles to the Sudoku board
      for i <- 0 until row do // is equal to y-coordinate

        // Displaying the y-coordinates in the GUI window
        val colText = new Label:
          text = " " + (i + 1) + " "
        colText.layoutY = 100
        grid.add(colText, 0, i + 1)
        GridPane.setValignment(colText, VPos.CENTER) // Centers the text (y-coordinate)
        // GridPane.setHalignment(colText, HPos.LEFT)

        for j <- 0 until col do // is equal to x-coordinate

          // Displaying the x-coordinates in the GUI window
          if i == 0 then
            val row = new Label:
              text = characters(j).toString
            grid.add(row, j + 1, 0)

            GridPane.setHalignment(row, HPos.CENTER) // Align tile row to center
          end if
          val rectangle = new Rectangle:
            x = 80
            y = 80
            width = 40
            height = 40
            fill = Color.LightGrey
          rectangle.setStroke(Color.Black)
          rectangle.setStrokeWidth(0.5)

          // When hovering the color changes to white
          rectangle.setOnMouseEntered( e => rectangle.setFill(Color.White))
          // When the cursor leaves the tile, the color of the tile will be the same as before
          rectangle.setOnMouseExited( e => rectangle.setFill(Color.LightGrey))
          tiles += rectangle

          grid.add(rectangle, j + 1, i + 1)
          // grid.setHgap(4.0)
          // grid.setVgap(4.0)
          // Drawing squares 3x3 to the board.
          /*if ((i + 1) % 3 == 0) && ((j + 1) % 3 == 0) then
              //val line = Line(rectangle.getX, rectangle.getY, col * rectangle.getWidth + rectangle.getX, row * rectangle.getHeight + rectangle.getY)
              val lineOnTop = new Line(rectangle.getX + ((i + 1) - 3) * rectangle.getWidth, rectangle.getY + ((j + 1) - 3) * rectangle.getHeight,
                rectangle.getX + ((i + 1) - 3) * rectangle.getWidth  + 3 * rectangle.getWidth, rectangle.getY + ((j + 1) - 3) * rectangle.getHeight)
              lineOnTop.setStrokeWidth(4.0)
              val lineOnBottom = new Line(rectangle.getX + ((i + 1) - 3) * rectangle.getWidth, rectangle.getY + (j + 1) * rectangle.getHeight,
                rectangle.getX + ((i + 1) - 3) * rectangle.getWidth  + 3 * rectangle.getWidth, rectangle.getY + (j + 1) * rectangle.getHeight)
              lineOnBottom.setStrokeWidth(4.0)
              val lineOnLeft = new Line(rectangle.getX + ((i + 1) - 3) * rectangle.getWidth, rectangle.getY + ((j + 1) - 3) * rectangle.getHeight,
                rectangle.getX + ((i + 1) - 3) * rectangle.getWidth, rectangle.getY + (j + 1) * rectangle.getHeight)
              lineOnLeft.setStrokeWidth(4.0)
              val lineOnRight = new Line(rectangle.getX + ((i + 1) - 3) * rectangle.getWidth  + 3 * rectangle.getWidth, rectangle.getY + ((j + 1) - 3) * rectangle.getHeight,
                rectangle.getX + ((i + 1) - 3) * rectangle.getWidth  + 3 * rectangle.getWidth, rectangle.getY + (j + 1) * rectangle.getHeight)
              lineOnRight.setStrokeWidth(4.0)
              root.children += lineOnTop
              root.children += lineOnBottom
              root.children += lineOnLeft
              root.children += lineOnRight
          end if*/
        end for
      end for
      root.children += grid
    // row and column of the board
*/

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
          // createGUIBoard(boardWithSize._2, boardWithSize._3)
          create3x3Squares(boardWithSize._2, boardWithSize._3)
        else
          assert(false)
        end if

      catch
        case e => e



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
    val input = new FileInputStream("src/images/left_arrow.PNG")
    val image = new Image(input)
    val imageViewUndo = new ImageView(image)

    val undoButton = Button(null, imageViewUndo)
      undoButton.scaleX = 0.5
      undoButton.scaleY = 0.5
      undoButton.layoutX = 40
      undoButton.layoutY = 450
      undoButton.setShape(new Circle(70))
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
