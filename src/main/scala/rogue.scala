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

    val room = new PlainRoom(this, cols, rows)

    val player = new Player(room.locs(10, 10))

    var command = new Command (this, room, player)

    var isPlaying: Boolean = false

    // Set up the elements of the user interface.
    def newGame: GridBagPanel = {
        val grid = new GridPanel(rows,cols)
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
        room.locs.map(_.update)

        listenTo(panel.keys);

        globalPanel = panel

        panel
    }

    def step {
        this.logs.text += "advance by 1 step\n"
        organisms.foreach(o => o.step(room))
        room.locs.map(_.battle)
        room.locs.map(_.update)
    }

    // User clicks on dungeon cell or item button ou type a command
    reactions += {
        case displayContents(p: Pos) => {
            this.logs.text += p.listContents
            this.cmdline.text += " " + p.x + " " + p.y
        }
        case leftClicked(o: Object) =>  { globalPanel.requestFocusInWindow() }
        case KeyPressed(_, c, _, _) =>  {
            c.toString match {
            case "Deux-points" => { cmdline.requestFocusInWindow() }
            // "?" => query(pos)
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
        contents = (new Castle).newGame
        centerOnScreen()
    }
}
// vim: set expandtab tabstop=4 shiftwidth=4 :
