package GUI

import javafx.scene.control.ToolBar
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.{Border, Pane, Background, TilePane}
import scalafx.scene.control.{Button, Label, Menu, MenuBar, MenuItem, SeparatorMenuItem}
import scalafx.Includes.*
import scalafx.geometry.Pos
import scalafx.scene.canvas.*
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color

object Main extends JFXApp3:

  def start(): Unit =
    stage = new JFXApp3.PrimaryStage:
      title = "Killer-Sudoku"
      width = 800
      height = 600

    val root = Pane()

    val scene = Scene(parent = root)
    stage.scene = scene


    // Adding tiles to the Sudoku board
    for i <- 0 until 9 do // is equal to y-coordinate
      for j <- 0 until 9 do // is equal to x-coordinate
        val rectangle = new Rectangle:
          x = 80
          y = 80
          width = 40
          height = 40
          fill = Color.LightGrey
        rectangle.setStroke(Color.Black)

        rectangle.setTranslateX(j * 40)
        rectangle.setTranslateY(i * 40)
        root.children += rectangle
      end for
    end for

    // Menu
    val menuBar = new MenuBar
    val fileMenu = new Menu("File")
    val newFileItem = new MenuItem("New file")
    newFileItem.onAction = (event) => println("New file -button in the menubar is clicked")
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
