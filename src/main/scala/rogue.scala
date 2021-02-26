import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.swing._
import java.awt.Font
import java.lang.System
import event._
import Direction._

/* Main game loop
 * - global application layout
 * - user interface
 * - move/battle/spawn coordination
 */

case class leftClicked (o: Object) extends Event
case class displayContents (p: Pos) extends Event

// BodyPart coordinates the gameplay. Mover is a thread that takes a target
// for the player and makes one move towards that target every 100ms. Mover
// stops if something "important" happens in between (such as a monster
// hitting the player), which is why a lot of the functions in the actor/
// item/floor code returns booleans.

class BodyPart extends Reactor {
    var globalPanel : GridBagPanel = null

    val cmdline = new TextField {
        columns = 32
        font = new Font("courier", 0, 17)
        background = Scheme.darkGray
        foreground = Scheme.white
    }
    val logs = new TextArea {
        font = new Font("courier", 0, 15)
        background = Scheme.darkGray
        foreground = Scheme.white
        editable = false
    }
    var organisms: Set[Organism] = Set()
    var items: Set[Item] = Set()
    var organismsBarycenter: Array[Pos] = Array(null, null)

    var organisms_selection: Set[Organism] = Set()
    var repeat: Int = 1

    val room = new Room(this, "cross")

    val player = new Player(room.locs(10, 10))

    var command = new Command (room)

    var isPlaying: Boolean = false

    // Set up the elements of the user interface.
    def newGame: GridBagPanel = {
        val grid = new GridPanel(room.rows, room.cols)
        room.locs.map(grid.contents += _)

        listenTo(room, cmdline);

        val panel = new GridBagPanel {
            def constraints (x: Int, y: Int,
                gridwidth: Int = 1, gridheight: Int = 1,
                weightx: Double = 0.0, weighty: Double = 0.0,
                fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None
            ): Constraints = {
                val c = new Constraints
                c.gridx = x
                c.gridy = y
                c.gridwidth = gridwidth
                c.gridheight = gridheight
                c.weightx = weightx
                c.weighty = weighty
                c.fill = fill
                c
            }
            add(grid, constraints(0, 0, gridheight=3, weightx=0.6, weighty=1.0, fill=GridBagPanel.Fill.Both))
            add(cmdline, constraints(1, 2, weightx=0.4, fill=GridBagPanel.Fill.Horizontal))
            add(new ScrollPane(logs) { preferredSize = new Dimension(30, 50) }, constraints(1, 1, weightx=0.3, weighty=1.0, fill=GridBagPanel.Fill.Both))
            add(Button("Close") { sys.exit(0) }, constraints(1, 0, weightx=0.4, fill=GridBagPanel.Fill.Horizontal))
            focusable = true;
        }
        panel.foreground = Scheme.darkGray
        panel.background = Scheme.darkGray
        room.locs.map(_.updateVisuals)

        listenTo(panel.keys);

        globalPanel = panel

        panel
    }

    // main loop
    def step {
        println("next turn")
        organisms.foreach(o => o.stats.syncCurrent)
        val () = { // update barycenter
            var count: Array[Int] = Array(0, 0)
            val i = Array(0, 0)
            val j = Array(0, 0)
            for (o <- organisms) {
                val idx = if (o.isFriendly) 1 else 0
                i(idx) += o.position.i
                j(idx) += o.position.j
                count(idx) += 1
            }
            for (idx <- 0 to 1) {
                organismsBarycenter(idx) = room.locs(i(idx) / count(idx).max(1), j(idx) / count(idx).max(1))
            }
        }
        var active = true
        // loop until no one can move anymore
        while (active) {
            // println("Still some active cells")
            active = false
            organisms.foreach(o => active = o.step(room) || active)
            room.locs.map(_.battle)
        }
        // items progress
        items.foreach(_.step)
        // viruses age
        organisms.foreach(o => {
            if (o.isFriendly && Rng.choice(0.07)) o.stats.health.residual -= 1
            o.sync
        })
        room.locs.map(_.trySpawn(organisms.size))
        room.locs.map(_.updateVisuals)
    }

    // User clicks on dungeon cell or item button ou type a command
    reactions += {
        case displayContents(p: Pos) => { command.locsClicked(p); command.commandRequest(this.cmdline.text) }
        case leftClicked(o: Object) =>  { globalPanel.requestFocusInWindow() }
        case KeyPressed(_, c, _, _) =>  { command.keyPressed(c) }
        case EditDone(`cmdline`) => { command.commandRequest(this.cmdline.text) }
    }

}

object main extends SimpleSwingApplication {
    val top = new MainFrame {
        title = "BodyPart"
        contents = ( new BodyPart ).newGame
        centerOnScreen()
    }
}
