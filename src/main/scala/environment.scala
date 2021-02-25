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

/* Environment: tiles of the "dungeon"
 * - visual feedback for tile contents
 * - interaction with organisms
 * - battle procedure
 * - initial layout of the room
 */

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

// one tile
class Pos (val room: Room, val i: Int, val j: Int) extends Button {
    var isFocused: Boolean = false // position of cursor
    // for all members of type Array[...] with two indexes, information
    // on friendly organisms is stored in (1) and hostile in (0)
    // This explains the `if (isFriendly) 1 else 0` in what follows
    var organisms: Array[Set[Organism]] = Array(Set(), Set())
    var items: Set[Item] = Set()
    var strength: Array[Int] = Array(0, 0)
    var blocking: Array[SkillRecord] = Array(new SkillRecord(), new SkillRecord())
    var friendlySpawner: PhysicalSpawner = null
    var hostileSpawner: PhysicalSpawner = null
    var notifyLevel: Int = 0 // visual feedback for important events

    this.focusable = false

    def setItem (i: Item) = {
       items.add(i)
       notification
    }
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
        room.body.organisms.remove(o)
    }

    // Vector addition on positions may fail if the resulting position
    // is outside of the grid
    def tryAdd (i: Direction): Pos = {
        val dpos = Direction.toTuple(i)
        val newJ = this.j + dpos._1
        val newI = this.i + dpos._2
        if (room.cols > newJ && room.rows > newI && 0 <= newJ && 0 <= newI) {
            room.locs(newI, newJ)
        } else null
    }
    def jump (vert: Int, horiz: Int) : Pos = {
        val newJ = this.j + horiz
        val newI = this.i + vert
        if (room.cols > newJ && room.rows > newI && 0 <= newJ && 0 <= newI) {
            room.locs(newI, newJ)
        } else null
    }

    // distances on locations
    def distanceL1 (other: Pos): Int = {
        (this.i - other.i).abs + (this.j - other.j).abs
    }
    def distanceL2 (other: Pos): Double = {
        sqrt(pow(this.i - other.i, 2) + pow(this.j - other.j, 2))
    }

    override def toString: String = {
        "(" + i + "," + j + ")"
    }

    // interaction with spawners
    def setFriendlySpawner (s: PhysicalSpawner) { friendlySpawner = s; s.position = this }
    def setHostileSpawner (s: PhysicalSpawner) { hostileSpawner = s; s.position = this }
    def trySpawn {
        if (friendlySpawner != null) friendlySpawner.step
        if (hostileSpawner != null) hostileSpawner.step
    }
    def forceSpawn {
        if (friendlySpawner != null) friendlySpawner.spawn
        if (hostileSpawner != null) hostileSpawner.spawn
    }

    // visual appearance
    border = createEmptyBorder
    preferredSize = new Dimension(20, 20)
    font = new Font("default", 0, 20)
    focusPainted = false

    // visual effects
    def notification {
        notifyLevel = 255
    }
    def updateVisuals {
        // text
        val totalStrength = strength(0) + strength(1)
        var t0 = if (totalStrength > 0) strength(0).toString else ""
        var t1 = if (totalStrength > 0) strength(1).toString else ""
        if (hostileSpawner != null) {
            t0 += "+"
            if (friendlySpawner == null && totalStrength == 0) t1 += "."
        }
        if (friendlySpawner != null) {
            t1 += "+"
            if (hostileSpawner == null && totalStrength == 0) t0 += "."
        }
        if (items.size != 0) t1 += "i"
        text = "<html><center>" + t1 + "<br>" + t0 + "</center></html>"
        // color
        background = Scheme.mix(Scheme.red, strength(0) / 100.0, Scheme.green, strength(1) / 100.0)
        background = Scheme.set_blue_channel(background, notifyLevel)
        notifyLevel = 3 * notifyLevel / 4
        if (isFocused) background = Scheme.white
        var bgShade = (background.getRed + background.getBlue + background.getGreen) / (255 * 3.0)
        foreground = if (bgShade > 0.5) Scheme.black else Scheme.white
    }

    def battle {
        // For now, battles are nondeterministic.
        // Each organism in turn picks an ennemy that is still alive and tries to attack it.
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
        // battles happen in a random order
        orgs = Rng.shuffle(orgs)
        split(0) = Rng.shuffle(split(0))
        split(1) = Rng.shuffle(split(1))
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
                        val swap = Rng.uniform(0, n - 1)
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
        if (items.size > 0) {
            items.foreach(i => {s += "  Item" +  i + "\n"})
        }
        if (organisms(0).size + organisms(1).size == 0) {
            s += "  empty\n"
        }
        s
    }
}

// Aggregate functions for positions
class Grid (room: Room, rows: Int, cols: Int) {
    val elem = IndexedSeq.tabulate(rows, cols) { (i, j) => new Pos(room, i, j) }
    def map[U] (f: Pos => U) = elem.map(_.map(f(_)))
    def filter (f: Pos => Boolean) = elem.flatten.filter(f(_))
    def apply (i: Int, j: Int) = elem(i)(j)
}
