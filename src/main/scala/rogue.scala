import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import scala.swing.BorderPanel.Position._
import javax.swing.BorderFactory._
import java.awt.{ Color, Font }
import java.lang.System
import event._

/****************************************************************************/

case class leftClicked (o:Object) extends Event
case class moveTo (c:Cell) extends Event

/****************************************************************************/

/****************************************************************************/

abstract class Floor
{
    def color:Color

    // true if entering ought to stop auto-move; block by default
    def enter(actor:Actor,cell:Cell) : Boolean = { return true }
}

// Wall and Ground are a single object to avoid instantiating one Floor
// object for each cell.

object Wall extends Floor { val color = Color.red }

object Ground extends Floor
{
    val color = Color.black

    // no problem entering here
    override def enter(actor:Actor,cell:Cell) : Boolean =
    {
        // move the actor
        actor.position.removeActor
        actor.placeOnMap(cell)
        return false
    }
}

/****************************************************************************/

// This class represents one dungeon cell with a floor, optionally an actor
// and an item. Handles the visual appearance and user-interface aspects.

class Cell (val room:Room, val y:Int, val x:Int) extends Button
{
    var floor : Floor = null
    var actor : Actor = null

    def setFloor (f:Floor) = { floor = f; update }
    def removeActor () = { actor = null; update }
    def setActor (a:Actor) { actor = a; update }

    // number of moves towards c
    def linfdist (c:Cell) = max(abs(c.x-x),abs(c.y-y))

    // Euclidian distance towards c
    def l2dist (c:Cell) = sqrt((c.x-x)*(c.x-x)+(c.y-y)*(c.y-y))

    // Return whether the cell has a normal floor and no actor or item.
    def isFree : Boolean =
    {
        if (actor != null) return false
        floor match { case Ground => return true }
        return false
    }

    // visual appearance
    border = createEmptyBorder
    font = new Font("default",0,25)
    preferredSize = new Dimension(20,20)
    focusPainted = false

    def update
    {
        def copySymbol (symbol:Symbol)
        {
            foreground = symbol.color
            text = symbol.form.toString
        }

        text = ""; background = floor.color
        if (actor != null) copySymbol(actor.symbol)
    }

    // user interface
    listenTo(mouse.clicks)

    reactions +=
    {
        case MouseClicked(_, _ ,0, _ , _ ) =>
            { publish(leftClicked(this)) }
        case UIElementResized(_) =>
            font = new Font("default",Font.BOLD,
                min(size.height,size.width)*4/5)
    }

}

// Handle a grid of dungeon cells, facilitating some aggregate functions
class Grid (room:Room, rows:Int, cols:Int)
{
    val elem = IndexedSeq.tabulate(rows,cols) {(i,j) => new Cell(room,i,j)}
    def map[U](f:Cell=>U) = elem.map(_.map(f(_)))
    def filter(f:Cell=>Boolean) = elem.flatten.filter(f(_))
    def apply (i:Int,j:Int) = elem(i)(j)
    def neighbours(c:Cell) = elem.flatten.filter(c.linfdist(_) <= 1)
}

/****************************************************************************/

// We only ever instantiate one single room, so some of the code here
// is overkill.

class Room (val castle:Castle, val cols:Int, val rows:Int)
    extends Reactor with Publisher
{
    var cells = new Grid(this,rows,cols)

    cells.map(_.setFloor(Ground))
    cells.map(listenTo(_))

    reactions +=
    {
        case leftClicked(c:Cell) => { publish(moveTo(c)) }
    }

    def makeWall (c:Cell, d:Cell)
    {
        for (x <- c.x to d.x; y <- c.y to d.y)
            { cells(y,x).setFloor(Wall) }
    }
}

class PlainRoom (castle:Castle, cols:Int, rows:Int)
    extends Room (castle,cols,rows)
{
    makeWall(cells(0,0),cells(0,cols-1))
    makeWall(cells(0,0),cells(rows-1,0))
    makeWall(cells(rows-1,0),cells(rows-1,cols-1))
    makeWall(cells(0,cols-1),cells(rows-1,cols-1))
}

/****************************************************************************/

// Castle coordinates the gameplay. Mover is a thread that takes a target
// for the player and makes one move towards that target every 100ms. Mover
// stops if something "important" happens in between (such as a monster
// hitting the player), which is why a lot of the functions in the actor/
// item/floor code returns booleans.

class Castle extends Reactor
{
    var cols = 25
    val rows = 25

    val hero = new Hero

    object Mover extends Runnable
    {
        var target : Cell = null

        def set_target (cell:Cell)
        {
            this.synchronized { target = cell }
        }

        def nextMove : Unit =
        {
            val hp = hero.position
            if (target == hp) return

            // find neighbour cell closest to target
            val neighbours = room.cells.neighbours(hp)
            val next = neighbours.minBy(_.l2dist(target))
        }

        def run
        {
            while (true)
            {
                this.synchronized { nextMove }
                Thread.sleep(100)
            }
        }
    }

    // Setting up the playing arena (except the hero, which is
    // placed later due to interference with Mover code).
    val room = new PlainRoom (this,cols,rows)
    {
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
    def resetHero (cell:Cell)
    {
        hero.placeOnMap(cell)
        Mover.set_target(cell)
    }

    // Set up the elements of the user interface.
    def newGame : GridBagPanel =
    {
        val grid = new GridPanel(rows,cols)
        room.cells.map(grid.contents += _)

        resetHero(room.cells(9,12))
        new Thread(Mover).start

        listenTo(room);

        val panel = new GridBagPanel {
            def constraints(x: Int, y: Int,
                gridwidth: Int = 1, gridheight: Int = 1,
                weightx: Double = 0.0, weighty: Double = 0.0,
                fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None
            ) : Constraints = {
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
            add(grid,
                constraints(0, 0, gridheight=3, weightx=1.0, weighty=1.0, fill=GridBagPanel.Fill.Both))
            add(new TextField {
                    columns = 32
                    font = new Font("default", 0, 17)
                    background = Color.darkGray
                    foreground = Color.white
                },
                constraints(1, 2, weightx=0.3, fill=GridBagPanel.Fill.Horizontal))
            add(new ScrollPane(new TextArea {
                    font = new Font("default", 0, 19)
                    background = Color.darkGray
                    foreground = Color.white
                }),
                constraints(1, 1, weighty = 1.0, fill=GridBagPanel.Fill.Both))
            add(Button("Close") { sys.exit(0) },
               constraints(1, 0, fill=GridBagPanel.Fill.Horizontal))
        }
        panel.foreground = Color.darkGray
        panel.background = Color.darkGray
        panel
    }

    // User clicks on dungeon cell or item button
    reactions +=
    {
        case moveTo(c:Cell) => { Mover.set_target(c) }
    }
}

/****************************************************************************/

object main extends SimpleSwingApplication
{
    val top = new MainFrame
    {
        title = "Castle"
        contents = (new Castle).newGame
        centerOnScreen()
    }
}
