package GUI

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.control.{Label, MenuBar, MenuItem, Menu, SeparatorMenuItem}
import scalafx.scene.control.Button
import scalafx.Includes._

object Main extends JFXApp3:

  def start(): Unit =
     stage = new JFXApp3.PrimaryStage:
      title = "Killer-Sudoku"
      width = 800
      height = 600
      scene = new Scene(800, 600) {
        // Menu
        val menuBar = new MenuBar
        val fileMenu = new Menu("File")
        val openItem = new MenuItem("Open file")
        openItem.onAction = (event) => println("Open file -button in the menubar is clicked")
        val saveItem = new MenuItem("Save file")
        saveItem.onAction = (event) => println("Save file -button in the menubar is clicked")

        fileMenu.items = List(openItem, saveItem)
        menuBar.menus = List(fileMenu)

        // Start Again -button
        val startAgainButton = Button("Start Again")
        startAgainButton.onAction = (event) => println("Clicked Start again -button")
        startAgainButton.layoutX = 500
        startAgainButton.layoutY = 0

        // Adding menu and Start Again -button to the content
        content = List(menuBar, startAgainButton)
      }


end Main
