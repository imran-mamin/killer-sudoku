package sudoku
package GUI

import javafx.scene.control.{ListCell, ListView}
import javafx.scene.input.MouseEvent

// Create a custom ListCell that changes the background color when the mouse enters and exits the cell
class CustomListCell extends ListCell[String]:
  override def updateItem(item: String, empty: Boolean): Unit = {
    super.updateItem(item, empty)
    if (empty || item == null) then
      setText(null)
      setGraphic(null)
    else
      setText(item)
  }

  // Add mouse event handlers to change the background color of the cell
  setOnMouseEntered((event: MouseEvent) => {
    if (!isEmpty) then
      setStyle("-fx-background-color: #1e90ff;")
  })

  setOnMouseExited((event: MouseEvent) => {
    setStyle("")
  })

end CustomListCell


