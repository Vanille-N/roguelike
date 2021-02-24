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
