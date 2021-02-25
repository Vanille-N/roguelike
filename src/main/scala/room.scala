import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.swing._
import javax.swing.BorderFactory._
import java.awt.Font
import java.lang.System
import event._
import java.io.FileNotFoundException
import java.io.IOException
import scala.io.Source

// The whole room
class Room (val body: BodyPart, fbase: String)
extends Reactor with Publisher {
    // initialization from src file
    var rows = 0
    var cols = 0
    var locs = new Grid(this, 0, 0)

    val wallSpawner = new DefaultWallCellSpawner()
    def makeWall (p: Pos, q: Pos) = {
        for (i <- p.i to q.i; j <- p.j to q.j) {
            wallSpawner.spawn(locs(i, j))
        }
    }
    val redCellSpawner = new DefaultRedCellSpawner()
    val whiteCellSpawner = new DefaultWhiteCellSpawner()
    val virusSpawner = new DefaultVirusSpawner()

    def init {
        val src = Source.fromFile("assets/" + fbase + ".room")
        val lines = src.getLines
        val dimensions = lines.next.split(" ")
        rows = dimensions(0).toInt
        cols = dimensions(1).toInt
        locs = new Grid(this, rows, cols)
        for (i <- 0 to rows-1) {
            val line = lines.next
            for (j <- 0 to cols-1) {
                line(j) match {
                    case ' ' => ()
                    case '*' => locs(i, j).setFriendlySpawner(new PhysicalSpawner(virusSpawner, 0.015, 10))
                    case '#' => wallSpawner.spawn(locs(i, j))
                    case 'R' => locs(i, j).setHostileSpawner(new PhysicalSpawner(redCellSpawner, 0.03, 7))
                    case 'W' => locs(i, j).setHostileSpawner(new PhysicalSpawner(whiteCellSpawner, 0.02, 5))
                }
            }
        }
        src.close
        val pathFinder = new PathFinder(availability, rows, cols)
    }
    init

    locs.map(listenTo(_))

    reactions += {
        case leftClicked(c: Pos) => { publish(displayContents(c)) }
    }

    def addOrganism (o: Organism, p: Pos) = {
        o.updateStrength
        o.placeOnMap(p)
        body.organisms.add(o)
    }

    def addItem (i: Item, p: Pos) = {
        body.items.add(i)
        i.setPos(p)
    }
}
