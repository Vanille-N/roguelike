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
    val wallSpawner = new DefaultWallCellSpawner()
    val redCellSpawner = new DefaultRedCellSpawner()
    val whiteCellSpawner = new DefaultWhiteCellSpawner()
    val virusSpawner = new DefaultVirusSpawner()

    // initialization from src file
    val (rows, cols, locs, pathFinder) = {
        val src = Source.fromFile("assets/" + fbase + ".room")
        val lines = src.getLines
        val dimensions = lines.next.split(" ")
        val rows = dimensions(0).toInt
        val cols = dimensions(1).toInt
        val locs = new Grid(this, rows, cols)
        val availability = Array.ofDim[Boolean](rows, cols)
        for (i <- 0 to rows - 1; j <- 0 to cols - 1) availability(i)(j) = true
        for (i <- 0 to rows - 1) {
            val line = lines.next
            for (j <- 0 to cols-1) {
                line(2*j) match {
                    case ' ' => ()
                    case '*' => locs(i, j).setFriendlySpawner(new PhysicalSpawner(virusSpawner, 0.015, 10))
                    case '#' => { wallSpawner.spawn(locs(i, j)); availability(i)(j) = false }
                    case 'R' => locs(i, j).setHostileSpawner(new PhysicalSpawner(redCellSpawner, 0.03, 7))
                    case 'W' => locs(i, j).setHostileSpawner(new PhysicalSpawner(whiteCellSpawner, 0.02, 5))
                }
            }
        }
        for (i <- 0 to rows - 1; j <- 0 to cols - 1) locs(i, j).forceSpawn
        src.close
        val pathFinder = new PathFinder(availability, rows, cols)
        (rows, cols, locs, pathFinder)
    }

    locs.map(listenTo(_))

    reactions += {
        case leftClicked(c: Pos) => { publish(displayContents(c)) }
    }

    def makeWall (p: Pos, q: Pos) = {
        for (i <- p.i to q.i; j <- p.j to q.j) {
            wallSpawner.spawn(locs(i, j))
        }
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
