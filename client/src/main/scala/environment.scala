import javax.swing.BorderFactory._
import java.awt.Font

import swing._
import event._

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
        val totalStrength = dual.strengthSelf + dual.strengthOther + dual.strengthCells
        var t0 = if (totalStrength > 0) dual.strengthSelf.toString else ""
        var t1 = if (totalStrength > 0) (dual.strengthOther + dual.strengthCells).toString else ""
        if (dual.hasHostileSpawner || dual.hasNeutralSpawner) { // '+' indicates a spawner
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
        background = Scheme.mix(List(
            (Scheme.green, dual.strengthSelf / 100.0),
            (Scheme.red, dual.strengthCells / 100.0),
            (Scheme.purple, dual.strengthOther / 100.0)
        ))
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
                (size.width / dual.strengthSelf.toString.length.max((dual.strengthOther + dual.strengthCells).toString.length).max(3)).min(
                size.height / 2))
        }
    }
}

// Aggregate functions for positions
class DisplayRoom (room: LocalRoom) {
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
