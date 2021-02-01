import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.swing._
import javax.swing.BorderFactory._
import java.awt.{ Color, Font }
import java.lang.System
import event._

import Alignment._

abstract class Floor {
    def color: Color
}

// Wall and Ground are a single object to avoid instantiating one Floor
// object for each cell.

object Wall extends Floor { val color = Color.red }

object Ground extends Floor {
    val color = Color.black
}

/****************************************************************************/

// This class represents one dungeon cell with a floor, optionally an actor
// and an item. Handles the visual appearance and user-interface aspects.

class Pos (val room: Room, val y: Int, val x: Int) extends Button {
    var floor: Floor = null
    var friendlyOrganisms: Set[Organism] = Set()
    var hostileOrganisms: Set[Organism] = Set()
    var neutralOrganisms: Set[Organism] = Set()
    var friendlyStrength: Int = 0
    var hostileStrength: Int = 0
    var neutralStrength: Int = 0

    def setFloor (f: Floor) = { floor = f; update }
    def removeOrgasism (alignment: Alignment, x: Organism) = {
        alignment match {
            case FRIENDLY => { friendlyOrganisms.remove(x); friendlyStrength -= x.strength }
            case HOSTILE => { hostileOrganisms.remove(x); hostileStrength -= x.strength }
            case NEUTRAL => { neutralOrganisms.remove(x); neutralStrength -= x.strength }
        }
    }
    def addOrganism (alignment: Alignment, x: Organism) = {
        alignment match {
            case FRIENDLY => { friendlyOrganisms.add(x); friendlyStrength += x.strength }
            case HOSTILE => { hostileOrganisms.add(x); hostileStrength += x.strength }
            case NEUTRAL => { neutralOrganisms.add(x); neutralStrength += x.strength }
        }
    }

    // visual appearance
    border = createEmptyBorder
    font = new Font("courier", 0, 25)
    preferredSize = new Dimension(20, 20)
    focusPainted = false

    def update {
        def copySymbol (symbol: Symbol) {
            foreground = symbol.color
            text = symbol.form.toString
        }

        text = ""; background = floor.color
    }

    // user interface
    listenTo(mouse.clicks)

    reactions += {
        case MouseClicked(_, _ ,0, _ , _ ) =>
            { publish(leftClicked(this)) }
        case UIElementResized(_) =>
            font = new Font("courier",Font.BOLD,
                min(size.height, size.width) * 4/5)
    }

}

// Handle a grid of dungeon cells, facilitating some aggregate functions
class Grid (room: Room, rows: Int, cols: Int) {
    val elem = IndexedSeq.tabulate(rows, cols) {(i, j) => new Pos(room, i, j)}
    def map[U] (f: Pos => U) = elem.map(_.map(f(_)))
    def filter (f: Pos => Boolean) = elem.flatten.filter(f(_))
    def apply (i: Int, j: Int) = elem(i)(j)
}

/****************************************************************************/

// We only ever instantiate one single room, so some of the code here
// is overkill.

class Room (val castle: Castle, val cols: Int, val rows: Int)
extends Reactor with Publisher {
    var locs = new Grid(this, rows, cols)

    locs.map(_.setFloor(Ground))
    locs.map(listenTo(_))

    reactions += {
        case leftClicked(c: Pos) => { publish(moveTo(c)) }
    }

    def makeWall (c: Pos, d: Pos) {
        for (x <- c.x to d.x; y <- c.y to d.y)
            { locs(y,x).setFloor(Wall) }
    }
}

class PlainRoom (castle: Castle, cols: Int, rows: Int)
extends Room (castle,cols,rows) {
    makeWall(locs(0, 0), locs(0, cols-1))
    makeWall(locs(0, 0), locs(rows-1, 0))
    makeWall(locs(rows-1, 0), locs(rows-1, cols-1))
    makeWall(locs(0, cols-1), locs(rows-1, cols-1))
}
