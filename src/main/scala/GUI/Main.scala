package GUI

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.Pane

object Main extends JFXApp3:

  def start(): Unit =
     stage = new JFXApp3.PrimaryStage:
      title = "Killer-Sudoku"
      width = 800
      height = 600

end Main
