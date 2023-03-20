package GUI

import javafx.scene.control.{TextInputDialog, ToolBar}
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
import javafx.beans.binding.Bindings
import javafx.scene.shape.Circle
import scalafx.scene.image.{Image, ImageView}

import java.io.FileInputStream


object Main extends JFXApp3:


  def start(): Unit =
    stage = new JFXApp3.PrimaryStage:
      title = "Killer-Sudoku"
      width = 800
      height = 600

    val root = Pane()
/*
    val newFileView      = StackPane()
    val openPreviousView = StackPane()
    val saveAsView       = StackPane()
*/
    val mainScene = Scene(parent = root)
    stage.scene = mainScene

    val tiles: Buffer[Rectangle] = Buffer()

    val characters: List[Char] = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')

    // Adding tiles to the Sudoku board
    for i <- 0 until 9 do // is equal to y-coordinate

      // Displaying the y-coordinates in the GUI window
      val column = new Text:
        text = (i + 1).toString
        x = 65
        y = 100
      column.setTranslateY(i * 40)
      root.children += column

      for j <- 0 until 9 do // is equal to x-coordinate

        // Displaying the x-coordinates in the GUI window
        if i == 0 then
          val row = new Text:
            text = characters(j).toString
            x = 100
            y = 65
          row.setTranslateX(j * 40)
          root.children += row
        end if
        val rectangle = new Rectangle:
          x = 80
          y = 80
          width = 40
          height = 40
          fill = Color.LightGrey
        rectangle.setStroke(Color.Black)
        rectangle.setTranslateX(j * 40)
        rectangle.setTranslateY(i * 40)
        rectangle.setOnMouseEntered( e => rectangle.setFill(Color.White))
        rectangle.setOnMouseExited( e => rectangle.setFill(Color.LightGrey))
        tiles += rectangle
        root.children += rectangle
      end for
    end for


    // Menu
    val menuBar = new MenuBar
    val fileMenu = new Menu("File")
    val newFileItem = new MenuItem("New file")
    newFileItem.onAction = (event) =>
      println("New file -button in the menubar is clicked")
      val downloadFile = new TextInputDialog("path")
        downloadFile.initOwner(stage)
        downloadFile.title = "New file"
        downloadFile.headerText = ""
        downloadFile.contentText = "Please, provide filepath here: "
        downloadFile.getDialogPane.setMinSize(400, 350)
      val result = downloadFile.showAndWait()
      result match
        case filepath      => println("None")
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

    // Previous step -button

    val input = new FileInputStream("src/images/left_arrow.PNG")
    val image = new Image(input)
    val imageView = new ImageView(image)

    val previousButton = Button(null, imageView)
    previousButton.scaleX = 0.5
    previousButton.scaleY = 0.5
    previousButton.layoutY = 450
    previousButton.layoutX = 40
    previousButton.setShape(new Circle(70))
    root.children += previousButton

  /*

      // Canvas (Sudoku-board)
      val canvas = Canvas(400, 400)
      val gc = canvas.graphicsContext2D

      // val toolBar = ToolBar
      // val combinationsLabel = Label

      // Adding menu and Start Again -button to the content
      content = List(menuBar, startAgainButton)

      } */


end Main
