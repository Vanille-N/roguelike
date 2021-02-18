import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.swing._
import java.awt.Font
import java.lang.System
import event._
import Direction._

/****************************************************************************/

case class leftClicked (o: Object) extends Event
case class displayContents (p: Pos) extends Event

/****************************************************************************/

class Symbol (val form: Char, val color: Color) {}

/****************************************************************************/

// Castle coordinates the gameplay. Mover is a thread that takes a target
// for the player and makes one move towards that target every 100ms. Mover
// stops if something "important" happens in between (such as a monster
// hitting the player), which is why a lot of the functions in the actor/
// item/floor code returns booleans.

class Castle extends Reactor {
    val cols = 25
    val rows = 25

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

    val room = new PlainRoom(this, rows, cols)

    val player = new Player(room.locs(10, 10))

    var command = new Command (this, room, player)

    var isPlaying: Boolean = false

    // Set up the elements of the user interface.
    def newGame: GridBagPanel = {
        val grid = new GridPanel(rows, cols)
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

    def step {
        println("next turn")
        organisms.foreach(o => o.stats.syncCurrent)
        var active = true
        while (active) {
            println("Still some active cells")
            active = false
            organisms.foreach(o => active = o.step(room) || active)
            room.locs.map(_.battle)
        }
        organisms.foreach(o => {
            if (o.isFriendly && Rng.choice(0.07)) o.stats.health.residual -= 1
            o.sync
        })
        items.foreach(i => {
            i.step
        })
        room.locs.map(_.trySpawn)
        room.locs.map(_.updateVisuals)
    }

    // User clicks on dungeon cell or item button ou type a command
    reactions += {
        case displayContents(p: Pos) => {
            this.logs.text += p.listContents
            if (this.cmdline.text != "") {
                this.cmdline.text += " " + p.i + " " + p.j
            }
        }
        case leftClicked(o: Object) =>  { globalPanel.requestFocusInWindow() }
        case KeyPressed(_, c, _, l) =>  {
            c match {
            case Key.Semicolon => { cmdline.requestFocusInWindow() }
            case Key.Colon   => { cmdline.requestFocusInWindow() }
            case Key.Numpad0 => {command.commandRequest("0")}
            case Key.Numpad1 => {command.commandRequest("1")}
            case Key.Numpad2 => {command.commandRequest("2")}
            case Key.Numpad3 => {command.commandRequest("3")}
            case Key.Numpad4 => {command.commandRequest("4")}
            case Key.Numpad5 => {command.commandRequest("5")}
            case Key.Numpad6 => {command.commandRequest("6")}
            case Key.Numpad7 => {command.commandRequest("7")}
            case Key.Numpad8 => {command.commandRequest("8")}
            case Key.Numpad9 => {command.commandRequest("9")}
            case Key.Escape => {command.commandRequest("Escape")}
            case Key.Up => {command.commandRequest("Up")}
            case Key.K => {command.commandRequest("Up")}
            case Key.Down => {command.commandRequest("Down")}
            case Key.J => {command.commandRequest("Down")}
            case Key.Right => {command.commandRequest("Right")}
            case Key.L => {command.commandRequest("Right")}
            case Key.Left => {command.commandRequest("Left")}
            case Key.H => {command.commandRequest("Left")}
            case Key.Q => {command.commandRequest("quit")}
            case Key.P => {command.commandRequest("Space")}
            case Key.Space => {command.commandRequest("Space")}
            case Key.N => {command.commandRequest("step")}
            case _ => { command.commandRequest(c.toString) }
            }
        }
        case EditDone(`cmdline`) => { command.commandRequest(this.cmdline.text) }
    }

}

/****************************************************************************/

object main extends SimpleSwingApplication {
    val top = new MainFrame {
        title = "Castle"
        contents = ( new Castle ).newGame
        centerOnScreen()
    }
}
