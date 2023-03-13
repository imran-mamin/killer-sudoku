package GUI

import javafx.scene.control.ToolBar
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.{Pane, Border}
import scalafx.scene.control.{Label, Menu, MenuBar, MenuItem, SeparatorMenuItem, Button}
import scalafx.Includes.*
import scalafx.geometry.Pos


object Main extends JFXApp3:

  def start(): Unit =
     stage = new JFXApp3.PrimaryStage:
      title = "Killer-Sudoku"
      width = 800
      height = 600

     val root = Pane()
     val scene = new Scene(parent = root) {
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

      // Start Again -button
      val startAgainButton = Button("Start Again")
      startAgainButton.onAction = (event) => println("Clicked Start again -button")
      startAgainButton.layoutX = 500
      startAgainButton.layoutY = 0
      // startAgainButton.alignment = Pos.TopCenter
      // startAgainButton.setMinSize(10, 10)
      startAgainButton.border = Border.stroke(2)


      // val toolBar = ToolBar
      // val combinationsLabel = Label

      // Adding menu and Start Again -button to the content
      content = List(menuBar, startAgainButton)
      }
      stage.scene = scene


end Main
