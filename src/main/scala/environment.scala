import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.swing._
import javax.swing.BorderFactory._
import java.awt.{ Color, Font }
import java.lang.System
import event._

/****************************************************************************/

// This class represents one dungeon cell with a floor, optionally an actor
// and an item. Handles the visual appearance and user-interface aspects.

object Direction extends Enumeration {
    type Direction = Value
    val UP = Value("up")
    val DOWN = Value("down")
    val LEFT = Value("left")
    val RIGHT = Value("right")

    def toTuple (i: Direction): Tuple2[Int, Int] = {
        i match {
            case UP => (0, -1)
            case DOWN => (0, 1)
            case LEFT => (-1, 0)
            case RIGHT => (1, 0)
        }
    }
}
import Direction._

class Pos (val room: Room, val y: Int, val x: Int) extends Button {
    var isFocused: Boolean = false
    var organisms: Array[Set[Organism]] = Array(Set(), Set())
    var strength: Array[Int] = Array(0, 0)
    var blocking: Array[SkillRecord] = Array(new SkillRecord(), new SkillRecord())

    def addOrganism (o: Organism) = {
        val idx = if (o.isFriendly) 1 else 0
        organisms(idx).add(o)
        strength(idx) += o.strength
        blocking(idx).addSkill(o.skills.blocking)
    }
    def removeOrganism (o: Organism) = {
        val idx = if (o.isFriendly) 1 else 0
        organisms(idx).remove(o)
        strength(idx) -= o.strength
        blocking(idx).removeSkill(o.skills.blocking)
    }

    def tryAdd (i: Direction): Pos = {
        val dpos = Direction.toTuple(i)
        val newX = this.x + dpos._1
        val newY = this.y + dpos._2
        if (room.cols > newX && room.rows > newY && 0 <= newX && 0 <= newY) {
            room.locs(newY, newX)
        } else null
    }

    // visual appearance
    border = createEmptyBorder
    font = new Font("courier", 0, 25)
    preferredSize = new Dimension(20, 20)
    focusPainted = false

    def update {
        text = ""; background = floor.color
        if (isFocused) background = Color.white
        // println(x, y, isFocused)
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

    def listContents: String = {
        var s = "At position (" + y + "," + x + ")\n"
        if (organisms(1).size > 0) {
            s += "    " + organisms(1).size + " virus\n"
            organisms(1).foreach(o => {
                s += "      " + o + "\n"
            })
        }
        if (organisms(0).size > 0) {
            s += "    " + organisms(0).size + " cells\n"
            organisms(0).foreach(o => {
                s += "      " + o + "\n"
            })
        }
        if (organisms(0).size + organisms(1).size == 0) {
            s += "    empty\n"
        }
        s
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

    locs.map(listenTo(_))

    reactions += {
        case leftClicked(c: Pos) => { publish(displayContents(c)) }
    }

    def addOrganism (o: Organism, p: Pos) = {
        o.updateStrength
        o.placeOnMap(p)
        castle.organisms.add(o)
    }

    def makeWall (p: Pos, q: Pos) = {
        for (x <- p.x to q.x; y <- p.y to q.y) {
            val o = new WallCell()
            addOrganism(o, locs(x, y))
        }
    }
}

class PlainRoom (castle: Castle, cols: Int, rows: Int)
extends Room (castle,cols,rows) {
    makeWall(locs(0, 0), locs(0, cols-1))
    makeWall(locs(0, 0), locs(rows-1, 0))
    makeWall(locs(rows-1, 0), locs(rows-1, cols-1))
    makeWall(locs(0, cols-1), locs(rows-1, cols-1))

    addOrganism(new RedCell(), locs(5, 5))
    addOrganism(new RedCell(), locs(5, 9))
    addOrganism(new WhiteCell(), locs(15, 15))
    addOrganism(new WhiteCell(), locs(15, 15))
    addOrganism(new WhiteCell(), locs(5, 5))
    addOrganism(new Virus(), locs(7, 2))
    addOrganism(new Virus(), locs(18, 3))
}
