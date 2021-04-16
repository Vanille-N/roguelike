import scala.swing._
import javax.swing.BorderFactory._
import java.awt.Font
import event._

/* Environment: tiles of the "dungeon"
 * - visual feedback for tile contents
 */

case class Notification (pos: Pos) extends Event
case class Focus (pos: Pos) extends Event
case class UnFocus (pos: Pos) extends Event

// one tile
class DisplayPos (val dual: Pos) extends Button {
    var isFocused: Boolean = false // position of cursor
    var notifyLevel: Int = 0 // visual feedback for important events

    listenTo(dual)
    val room = dual.room
    val i = dual.i
    val j = dual.j
    this.focusable = false

    override def toString: String = {
        "[" + i + "," + j + "]"
    }

    // visual appearance
    border = createEmptyBorder
    preferredSize = new Dimension(20, 20)
    font = new Font("default", 0, 20)
    focusPainted = false

    def updateVisuals {
        // text
        val totalStrength = dual.strength(0) + dual.strength(1)
        var t0 = if (totalStrength > 0) dual.strength(0).toString else ""
        var t1 = if (totalStrength > 0) dual.strength(1).toString else ""
        if (dual.hostileSpawner != null) { // '+' indicates a spawner
            t0 += "+"
            if (dual.friendlySpawner == null && totalStrength == 0) t1 += "."
        }
        if (dual.friendlySpawner != null) {
            t1 += "+"
            if (dual.hostileSpawner == null && totalStrength == 0) t0 += "."
        }
        if (dual.artefacts.size != 0) t1 += "A" // 'i' indicates an item
            text = "<html><center>" + t1 + "<br>" + t0 + "</center></html>" // html required to have multiline texs
        if (dual.items.size != 0) t1 += "i" // 'i' indicates an item
            text = "<html><center>" + t1 + "<br>" + t0 + "</center></html>" // html required to have multiline texs
        // color
        background = Scheme.mix(
            Scheme.red, dual.strength(0) / 100.0,
            Scheme.green, dual.strength(1) / 100.0
        )
        background = Scheme.setBlueChannel(background, notifyLevel)
        if (isFocused) background = Scheme.white
        var bgShade = (background.getRed + background.getBlue + background.getGreen) / (255 * 3.0)
        foreground = if (bgShade > 0.5) Scheme.black else Scheme.white
    }

    // user interface
    listenTo(mouse.clicks)
    listenTo(room.body)

    reactions += {
        case MouseClicked(_, _ ,0, _ , _ ) =>
            { publish(DisplayContents(dual)) }
        case UIElementResized(_) => {
            font = new Font("default", Font.BOLD,
                (size.width / dual.strength(0).toString.length.max(dual.strength(1).toString.length).max(3)).min(
                size.height / 2))
        }
        case LoopStep() => notifyLevel = 3 * notifyLevel / 4
        case Notification(_) => notifyLevel = 255
        case Focus(_) => isFocused = true
        case UnFocus(_) => isFocused = false
    }
}

// Aggregate functions for positions
class DisplayGrid (room: Room) {
    val rows = room.rows
    val cols = room.cols
    val elem = IndexedSeq.tabulate(rows, cols) {
        (i, j) => {
            new DisplayPos(room.locs(i,j))
        }
    }
    def map[U] (f: DisplayPos => U) = elem.map(_.map(f(_)))
    def filter (f: DisplayPos => Boolean) = elem.flatten.filter(f(_))
    def apply (i: Int, j: Int) = elem(i)(j)
}
