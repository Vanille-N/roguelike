import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.swing._
import java.awt.Font
import java.lang.System
import event._

/****************************************************************************/

case class leftClicked (o: Object) extends Event
case class displayContents (p: Pos) extends Event

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

    import Direction._
    def tryMove (prompt: String, dir: Direction) = {
        if (player.move(dir)) {
            this.logs.text += prompt + "Player goes " + dir
            this.logs.text += "    -> " + player.position.x + ", " + player.position.y + "\n"
        } else {
            this.logs.text += "\t> Player cannot go " + dir + "\n"
        }
    }

    def trySet (prompt: String, args: Array[String]) = {
        for (i <- args)
            this.logs.text += "argument: " + i + "\n"
        args(0) match {
            case "health" => {  }
            case "base_strength" => {  }
            case _ => {  }
        }
    }

    // User clicks on dungeon cell or item button ou type a command
    reactions += {
        case displayContents(p: Pos) => {
            this.logs.text += p.listContents
        }
        case KeyPressed(_, c, _, _) => {
            val prompt = "[" + c.toString + "]"
            c.toString match {
                case "Deux-points" => { cmdline.requestFocusInWindow() }
                case "Q" => { sys.exit(0) }
                case "K" => { tryMove(prompt, UP) }
                case "J" => { tryMove(prompt, DOWN) }
                case "L" => { tryMove(prompt, RIGHT) }
                case "H" => { tryMove(prompt, LEFT) }
                // "?" => query(pos)
                case _ => {}
            }
            room.locs.map(_.update)
        }
        case EditDone(`cmdline`) => {
            if (this.cmdline.text != "") this.logs.text += "$ " + this.cmdline.text;
            val prompt = "\t>"
            this.cmdline.text.split(" ")(0) match {
                case "Up" => tryMove(prompt, UP)
                case "Down" => tryMove(prompt, DOWN)
                case "Right" => tryMove(prompt, RIGHT)
                case "Left" => tryMove(prompt, LEFT)
                case "quit" => { sys.exit(0) }
                case "q" => { globalPanel.requestFocusInWindow() }
                case "clear" => { this.logs.text = "" }
                case "set" => { trySet("setting", cmdline.text.substring(3).split(" ")) }
                case "" => {}
                case _ => { this.logs.text += "\t> command not found ;/\n" }
            }
            this.cmdline.text = "";
            room.locs.map(_.update)
        }
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
