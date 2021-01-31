import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import javax.swing.BorderFactory._
import java.awt.{ Color, Font }
import java.lang.System
import event._

abstract class Floor {
    def color: Color

    // true if entering ought to stop auto-move; block by default
    def enter (actor: Actor, cell: Cell): Boolean = { return true }
}

// Wall and Ground are a single object to avoid instantiating one Floor
// object for each cell.

object Wall extends Floor { val color = Color.red }

object Ground extends Floor {
    val color = Color.black

    // no problem entering here
    override def enter (actor: Actor, cell: Cell): Boolean = {
        // move the actor
        actor.position.removeActor
        actor.placeOnMap(cell)
        return false
    }
}

/****************************************************************************/

// This class represents one dungeon cell with a floor, optionally an actor
// and an item. Handles the visual appearance and user-interface aspects.

class Cell (val room: Room, val y: Int, val x: Int) extends Button {
    var floor: Floor = null
    var actor: Actor = null

    def setFloor (f: Floor) = { floor = f; update }
    def removeActor () = { actor = null; update }
    def setActor (a: Actor) = { actor = a; update }

    // number of moves towards c
    def linfdist (c: Cell) = max(abs(c.x - x), abs(c.y - y))

    // Euclidian distance towards c
    def l2dist (c: Cell) = sqrt((c.x - x) * (c.x - x) + (c.y - y) * (c.y - y))

    // Return whether the cell has a normal floor and no actor or item.
    def isFree: Boolean = {
        if (actor != null) return false
        floor match { case Ground => return true }
        return false
    }

    // visual appearance
    border = createEmptyBorder
    font = new Font("default", 0, 25)
    preferredSize = new Dimension(20, 20)
    focusPainted = false

    def update {
        def copySymbol (symbol: Symbol) {
            foreground = symbol.color
            text = symbol.form.toString
        }

        text = ""; background = floor.color
        if (actor != null) copySymbol(actor.symbol)
    }

    // user interface
    listenTo(mouse.clicks)

    reactions += {
        case MouseClicked(_, _ ,0, _ , _ ) =>
            { publish(leftClicked(this)) }
        case UIElementResized(_) =>
            font = new Font("default",Font.BOLD,
                min(size.height, size.width) * 4/5)
    }

}

// Handle a grid of dungeon cells, facilitating some aggregate functions
class Grid (room: Room, rows: Int, cols: Int) {
    val elem = IndexedSeq.tabulate(rows, cols) {(i, j) => new Cell(room, i, j)}
    def map[U] (f: Cell => U) = elem.map(_.map(f(_)))
    def filter (f: Cell => Boolean) = elem.flatten.filter(f(_))
    def apply (i: Int, j: Int) = elem(i)(j)
    def neighbours (c: Cell) = elem.flatten.filter(c.linfdist(_) <= 1)
}

/****************************************************************************/

// We only ever instantiate one single room, so some of the code here
// is overkill.

class Room (val castle: Castle, val cols: Int, val rows: Int)
extends Reactor with Publisher {
    var cells = new Grid(this, rows, cols)

    cells.map(_.setFloor(Ground))
    cells.map(listenTo(_))

    reactions += {
        case leftClicked(c: Cell) => { publish(moveTo(c)) }
    }

    def makeWall (c: Cell, d: Cell) {
        for (x <- c.x to d.x; y <- c.y to d.y)
            { cells(y,x).setFloor(Wall) }
    }
}

class PlainRoom (castle: Castle, cols: Int, rows: Int)
extends Room (castle,cols,rows) {
    makeWall(cells(0, 0), cells(0, cols-1))
    makeWall(cells(0, 0), cells(rows-1, 0))
    makeWall(cells(rows-1, 0), cells(rows-1, cols-1))
    makeWall(cells(0, cols-1), cells(rows-1, cols-1))
}
