package GUI

import scalafx.scene.layout.StackPane
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color


class SingleTile extends StackPane:
  val rectangle = new Rectangle:
    x = 100
    y = 100
    width  = 50
    height = 50
    fill = Color.Blue
  
end SingleTile

