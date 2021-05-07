import scala.swing._
import javax.swing.BorderFactory._
import java.awt.Font
import event._

/* Environment: tiles of the "dungeon"
 * - visual feedback for tile contents
 */

case class Notification (i: Int, j: Int) extends Event

// one tile
class DisplayPos (val dual: LocalPos) extends Button {
    var isFocused: Boolean = false // position of cursor
    var notifyLevel: Int = 0 // visual feedback for important events

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
        notifyLevel = 3 * notifyLevel / 4
        if (dual.hasNotification) notifyLevel = 255
        isFocused = dual.needsFocus

        // text
        val totalStrength = dual.strength(0) + dual.strength(1)
        var t0 = if (totalStrength > 0) dual.strength(0).toString else ""
        var t1 = if (totalStrength > 0) dual.strength(1).toString else ""
        if (dual.hasHostileSpawner) { // '+' indicates a spawner
            t0 += "+"
            if (!dual.hasFriendlySpawner && totalStrength == 0) t1 += "."
        }
        if (dual.hasFriendlySpawner) {
            t1 += "+"
            if (!dual.hasHostileSpawner && totalStrength == 0) t0 += "."
        }
        // html required to have multiline text
        if (dual.hasArtefacts) t1 += "A" // 'A' indicates an artefact
        if (dual.hasItems) t1 += "i" // 'i' indicates an item
        text = "<html><center>" + t1 + "<br>" + t0 + "</center></html>"
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

    reactions += {
        case MouseClicked(_, _,0 , _, _) =>
            { publish(DisplayContents(dual.i, dual.j)) }
        case UIElementResized(_) => {
            font = new Font("default", Font.BOLD,
                (size.width / dual.strength(0).toString.length.max(dual.strength(1).toString.length).max(3)).min(
                size.height / 2))
        }
    }
}

// Aggregate functions for positions
class DisplayGrid (room: LocalRoom) {
    val rows = room.rows
    val cols = room.cols
    val elem = IndexedSeq.tabulate(rows, cols) {
        (i, j) => {
            new DisplayPos(room.locs(i)(j))
        }
    }
    def map[U] (f: DisplayPos => U) = elem.map(_.map(f(_)))
    def filter (f: DisplayPos => Boolean) = elem.flatten.filter(f(_))
    def apply (i: Int, j: Int) = elem(i)(j)
}

class LocalRoom (
    val rows: Int,
    val cols: Int,
) {
    val locs = IndexedSeq.tabulate(rows, cols) {
        (i, j) => new LocalPos(i, j)
    }

    def syncWithRoom (
        room: Room,
        focusPos: Tuple2[Int,Int],
        notifications: List[Tuple2[Int,Int]],
      ) {
        for (i <- 0 to rows-1; j <- 0 to cols-1) {
            var pos = locs(i)(j)
            var src = room.locs(i,j)
            pos.strength(0) = src.strength(0)
            pos.strength(1) = src.strength(1)
            pos.hasFriendlySpawner = (src.friendlySpawner != null)
            pos.hasHostileSpawner = (src.hostileSpawner != null)
            pos.hasArtefacts = (src.artefacts.size != 0)
            pos.hasItems = (src.items.size != 0)
            pos.needsFocus = (pos.i == focusPos._1 && pos.j == focusPos._2)
            pos.hasNotification = false
        }
        for (notif <- notifications) {
            locs(notif._1)(notif._2).hasNotification = true
        }
    }
}

class LocalPos (
    val i: Int,
    val j: Int,
) {
    var strength = Array(0, 0)
    var hasFriendlySpawner: Boolean = false
    var hasHostileSpawner: Boolean = false
    var hasArtefacts: Boolean = false
    var hasItems: Boolean = false

    var needsFocus: Boolean = false
    var hasNotification: Boolean = false
}
