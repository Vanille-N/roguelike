import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.{ Color, Font }
import java.lang.System
import event._

/****************************************************************************/

case class leftClicked (o: Object) extends Event
case class moveTo (c: Cell) extends Event

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

    val hero = new Hero

    object Mover extends Runnable {
        var target: Cell = null

        def set_target (cell: Cell) {
            this.synchronized { target = cell }
        }

        def nextMove: Unit = {
            val hp = hero.position
            if (target == hp) return

            // find neighbour cell closest to target
            val neighbours = room.cells.neighbours(hp)
            val next = neighbours.minBy(_.l2dist(target))
        }

        def run {
            while (true) {
                this.synchronized { nextMove }
                Thread.sleep(100)
            }
        }
    }

    val cmdline = new TextField {
        columns = 32
        font = new Font("default", 0, 17)
        background = Color.darkGray
        foreground = Color.white
    }
    val logs = new TextArea {
        font = new Font("default", 0, 19)
        background = Color.darkGray
        foreground = Color.white
        editable = false
    }

    // Setting up the playing arena (except the hero, which is
    // placed later due to interference with Mover code).
    val room = new PlainRoom (this, cols, rows) {
        cells(5,10).setFloor(Wall)
        cells(5,11).setFloor(Wall)
        cells(5,12).setFloor(Wall)
        cells(14,10).setFloor(Wall); cells(14,12).setFloor(Wall)
        cells(15,10).setFloor(Wall); cells(15,12).setFloor(Wall)
        cells(16,10).setFloor(Wall); cells(16,12).setFloor(Wall)

        (new Fairy).placeOnMap(cells(14,11))
        (new Gnome { setEnemy(hero) }).placeOnMap(cells(9,16))
        (new Gnome { setEnemy(hero) }).placeOnMap(cells(15,2))
    }

    // Change hero position (e.g. hero enters new room), where no
    // reaction from other actors is desired.
    def resetHero (cell: Cell) {
        hero.placeOnMap(cell)
        Mover.set_target(cell)
    }

    // Set up the elements of the user interface.
    def newGame: GridBagPanel = {
        val grid = new GridPanel(rows,cols)
        room.cells.map(grid.contents += _)

        resetHero(room.cells(9, 12))
        new Thread(Mover).start

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
        case moveTo(c: Cell) => { this.logs.text += "clicked at position (" + c.x + "," + c.y + ")\n" }
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
