package sudoku
package GUI

import javafx.scene.control.{ListCell, ListView}
import javafx.scene.input.MouseEvent
import javafx.scene.paint.Paint
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color

/** Create a custom ListCell that changes the background color to blue,
 *  when the mouse enters and changes it back to normal, when exits the cell
 */
class CustomListCell(tiles: Vector[Rectangle], board: Puzzleboard, colNSize: Int) extends ListCell[String]:

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

  /** This method converts the index in the gui to the one in the back-end of the program. */
  private def convertIndex(i: Int): Int =
    val amountOfSquaresHorizontal: Int = colNSize / 3
    val m = (i / 9) / amountOfSquaresHorizontal // row of the 3x3 square
    val n = (i / 9) % amountOfSquaresHorizontal // col of the 3x3 square
    val topLeft3x3 = m * amountOfSquaresHorizontal * 3 * 3 + n * 3 // index of the tile in top left corner in 3x3square
    ((i % 9) / 3) * amountOfSquaresHorizontal * 3 + (i % 9) % 3 + topLeft3x3
  end convertIndex


end CustomListCell


