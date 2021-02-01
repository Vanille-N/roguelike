import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.{ Color, Font }
import java.lang.System
import event._

/****************************************************************************/

case class leftClicked (o: Object) extends Event
case class moveTo (p: Pos) extends Event

/****************************************************************************/

class Symbol (val form: Char, val color: Color) {}

/****************************************************************************/

// Castle coordinates the gameplay. Mover is a thread that takes a target
// for the player and makes one move towards that target every 100ms. Mover
// stops if something "important" happens in between (such as a monster
// hitting the player), which is why a lot of the functions in the actor/
// item/floor code returns booleans.

class Castle extends Reactor {
    var cols = 25
    val rows = 25

    val cmdline = new TextField {
        columns = 32
        font = new Font("courier", 0, 17)
        background = Color.darkGray
        foreground = Color.white
    }
    val logs = new TextArea {
        font = new Font("courier", 0, 19)
        background = Color.darkGray
        foreground = Color.white
        editable = false
    }

    // Setting up the playing arena (except the hero, which is
    // placed later due to interference with Mover code).
    val room = new PlainRoom(this, cols, rows) {}

    // Set up the elements of the user interface.
    def newGame: GridBagPanel = {
        val grid = new GridPanel(rows,cols)
        room.locs.map(grid.contents += _)

        listenTo(room);

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
            add(grid, constraints(0, 0, gridheight=3, weightx=1.0, weighty=1.0, fill=GridBagPanel.Fill.Both))
            add(cmdline, constraints(1, 2, weightx=0.3, fill=GridBagPanel.Fill.Horizontal))
            add(new ScrollPane(logs), constraints(1, 1, weighty=1.0, fill=GridBagPanel.Fill.Both))
            add(Button("Close") { sys.exit(0) },
               constraints(1, 0, fill=GridBagPanel.Fill.Horizontal))
        }
        panel.foreground = Color.darkGray
        panel.background = Color.darkGray
        panel
    }

    // User clicks on dungeon cell or item button
    reactions += {
        case moveTo(p: Pos) => { this.logs.text += "clicked at position (" + p.x + "," + p.y + ")\n" }
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
