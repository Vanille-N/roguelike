import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.util.Random
import scala.swing._
import javax.swing.BorderFactory._
import java.awt.Font
import java.lang.System
import event._

/****************************************************************************/

// This class represents one dungeon cell with a floor, optionally an actor
// and an item. Handles the visual appearance and user-interface aspects.

object Direction extends Enumeration {
    type Direction = Value
    val UP = Value("up")
    val DOWN = Value("down")
    val LEFT = Value("left")
    val RIGHT = Value("right")
    val STAY = Value("stay")

    def toTuple (d: Direction): Tuple2[Int, Int] = {
        d match {
            case UP => (0, -1)
            case DOWN => (0, 1)
            case LEFT => (-1, 0)
            case RIGHT => (1, 0)
            case STAY => (0, 0)
        }
    }
}
import Direction._

class Pos (val room: Room, val i: Int, val j: Int) extends Button {
    var isFocused: Boolean = false
    var organisms: Array[Set[Organism]] = Array(Set(), Set())
    var strength: Array[Int] = Array(0, 0)
    var blocking: Array[SkillRecord] = Array(new SkillRecord(), new SkillRecord())
    var virusSpawner: PhysicalSpawner = null
    var cellSpawner: PhysicalSpawner = null

    this.focusable = false

    def addOrganism (o: Organism) = {
        val idx = if (o.isFriendly) 1 else 0
        organisms(idx).add(o)
        strength(idx) += o.strength
        blocking(idx).addSkill(o.skills.blocking)
    }
    def removeOrganism (o: Organism) = {
        val idx = if (o.isFriendly) 1 else 0
        organisms(idx).remove(o)
        strength(idx) -= o.strength
        blocking(idx).removeSkill(o.skills.blocking)
    }
    def kill (o: Organism) = {
        removeOrganism(o)
        room.castle.organisms.remove(o)
    }

    def tryAdd (i: Direction): Pos = {
        val dpos = Direction.toTuple(i)
        val newJ = this.j + dpos._1
        val newI = this.i + dpos._2
        if (room.cols > newJ && room.rows > newI && 0 <= newJ && 0 <= newI) {
            room.locs(newI, newJ)
        } else null
    }

    def distanceL1 (other: Pos): Int = {
        (this.i - other.i).abs + (this.j - other.j).abs
    }
    def distanceL2 (other: Pos): Double = {
        sqrt(pow(this.i - other.i, 2) + pow(this.j - other.j, 2))
    }

    override def toString: String = {
        "(" + i + "," + j + ")"
    }

    def setFriendlySpawner (s: PhysicalSpawner) { virusSpawner = s; s.position = this }
    def setHostileSpawner (s: PhysicalSpawner) { cellSpawner = s; s.position = this }
    def trySpawn {
        if (virusSpawner != null) virusSpawner.step
        if (cellSpawner != null) cellSpawner.step
    }

    // visual appearance
    border = createEmptyBorder
    preferredSize = new Dimension(20, 20)
    font = new Font("default", 0, 20)
    focusPainted = false

    def updateVisuals {
        // text
        var t0 = if (strength(0) + strength(1) > 0) strength(0).toString else " "
        var t1 = if (strength(0) + strength(1) > 0) strength(1).toString else " "
        if (cellSpawner != null) t0 += "+"
        if (virusSpawner != null) t1 += "+"
        text = "<html><center>" + t1 + "<br>" + t0 + "</center></html>"
        // color
        background = Scheme.mix(Scheme.red, strength(0) / 100.0, Scheme.green, strength(1) / 100.0)
        if (isFocused) background = Scheme.white
        var bgShade = (background.getRed + background.getBlue + background.getGreen) / (255 * 3.0)
        foreground = if (bgShade > 0.5) Scheme.black else Scheme.white
    }

    def battle {
        // For now, battles are nondeterministic.
        // Each organism in turn (in a random order) picks an ennemy that is still alive and tries to attack it.
        // This attack may fail due to skills.
        var orgs: Buffer[Organism] = Buffer()
        var split: Array[Buffer[Organism]] = Array(Buffer(), Buffer())
        for (i <- 0 to 1) {
            organisms(i).foreach(x => {
                if (x.stats.health.residual > 0) {
                    orgs.append(x); split(i).append(x)
                }
            })
        }
        val r = new Random()
        r.shuffle(orgs)
        r.shuffle(split(0))
        r.shuffle(split(1))
        orgs.foreach(x => {
            val idx = if (x.isFriendly) 0 else 1
            if (split(idx).size > 0) {
                // chose target
                val target = split(idx)(0)
                target.attackedBy(x)
                if (target.stats.health.residual <= 0) {
                    // target is dead, remove from attackable
                    if (split(idx).size > 1) {
                        val n = split(idx).size
                        split(idx)(0) = split(idx)(n - 1)
                    }
                    split(idx).trimEnd(1)
                } else {
                    // target is still alive, pick next target randomly
                    if (split(idx).size > 1) {
                        val n = split(idx).size
                        val swap = r.nextInt(n - 1)
                        val tmp = split(idx)(n - 1)
                        split(idx)(n - 1) = split(idx)(0)
                        split(idx)(0) = tmp
                    }
                }
            }
        })
    }

    // user interface
    listenTo(mouse.clicks)

    reactions += {
        case MouseClicked(_, _ ,0, _ , _ ) =>
            { publish(leftClicked(this)) }
        case UIElementResized(_) =>
            font = new Font("default", Font.BOLD,
                (size.width / strength(0).toString.length.max(strength(1).toString.length).max(3)).min(
                size.height / 2))
    }

    def listContents: String = {
        var s = "At position (" + i + "," + j + ")\n"
        var k = 0
        if (organisms(1).size > 0) {
            s += "  " + organisms(1).size + " virus\n"
            organisms(1).foreach(o => {
                s += "    " + k + "- " +  o + "\n"
                k += 1
            })
        }
        if (organisms(0).size > 0) {
            s += "  " + organisms(0).size + " cells\n"
            organisms(0).foreach(o => {
                s += "    " + k + "- " +  o + "\n"
                k += 1
            })
        }
        if (organisms(0).size + organisms(1).size == 0) {
            s += "  empty\n"
        }
        s
    }
}

class Grid (room: Room, rows: Int, cols: Int) {
    val elem = IndexedSeq.tabulate(rows, cols) {(i, j) => new Pos(room, i, j)}
    def map[U] (f: Pos => U) = elem.map(_.map(f(_)))
    def filter (f: Pos => Boolean) = elem.flatten.filter(f(_))
    def apply (i: Int, j: Int) = elem(i)(j)
}

/****************************************************************************/

class Room (val castle: Castle, val cols: Int, val rows: Int)
extends Reactor with Publisher {
    var locs = new Grid(this, rows, cols)

    locs.map(listenTo(_))

    reactions += {
        case leftClicked(c: Pos) => { publish(displayContents(c)) }
    }

    def addOrganism (o: Organism, p: Pos) = {
        o.updateStrength
        o.placeOnMap(p)
        castle.organisms.add(o)
    }

    def wallSpawner = new DefaultWallCellSpawner()

    def makeWall (p: Pos, q: Pos) = {
        for (i <- p.i to q.i; j <- p.j to q.j) {
            wallSpawner.spawn(locs(i, j))
        }
    }
}

class PlainRoom (castle: Castle, rows: Int, cols: Int)
extends Room (castle, rows, cols) {
    makeWall(locs(0, 0), locs(0, cols-1))
    makeWall(locs(0, 0), locs(rows-1, 0))
    makeWall(locs(rows-1, 0), locs(rows-1, cols-1))
    makeWall(locs(0, cols-1), locs(rows-1, cols-1))
    makeWall(locs(rows/2, cols/3), locs(cols/2, 2*cols/3))

    val redCellSpawner = new DefaultRedCellSpawner()
    locs(5, 5).setHostileSpawner(new PhysicalSpawner(redCellSpawner, 20))
    locs(5, 9).setHostileSpawner(new PhysicalSpawner(redCellSpawner, 30))

    val whiteCellSpawner = new DefaultWhiteCellSpawner()
    locs(15, 15).setHostileSpawner(new PhysicalSpawner(whiteCellSpawner, 10))
    locs(15, 15).setHostileSpawner(new PhysicalSpawner(whiteCellSpawner, 10))
    locs(5, 5).setHostileSpawner(new PhysicalSpawner(whiteCellSpawner, 10))

    val virusSpawner = new DefaultVirusSpawner()
    locs(7, 2).setFriendlySpawner(new PhysicalSpawner(virusSpawner, 10))
    locs(18, 3).setFriendlySpawner(new PhysicalSpawner(virusSpawner, 10))
}
