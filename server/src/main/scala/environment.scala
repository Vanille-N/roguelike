import Math._
import scala.collection.mutable.{ Buffer, Set }

import swing._
import event._

/* Environment: tiles of the "dungeon"
 * - interaction with organisms
 * - battle procedure
 */

object Direction extends Enumeration { // allowed moves for organisms
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
class Pos (val room: Room, val i: Int, val j: Int, nbPlayers: Int)
extends Publisher with Reactor {
    // for all members of type Array[...] with two indexes, information
    // on friendly organisms is stored in (1) and hostile in (0)
    // If there are more indexes it is usually cells in (0) and
    // each player's viruses in (player.id)
    var organisms: Array[Set[Organism]] = Array.fill(2) { Set() } // all organisms
    // organisms(0) -> non friendly organisms (cells)
    // organisms(k) -> friendly organisms (viruses of player k)
    var items: Set[Item] = Set() // all items that are on the floor
    var artefacts: Set[Artefact] = Set() // all items that are on the floor
    var strength: Array[Int] = Array.fill(nbPlayers+1) { 0 } // arbitrary measure of strength
    var blocking: Array[SkillRecord] = Array.fill(2) { new SkillRecord() } // are cells allowed to step over this tile ?
    var spawner: Array[PhysicalSpawner] = Array.fill(nbPlayers+1) { null }

    def notification {
        publish(Notification(i, j))
    }

    def addOrganism (o: Organism) = { // organism enters the tile
        if (o != null) {
            organisms(o.asBinary).add(o)
            strength(o.asIndex) += o.strength
            blocking(o.asBinary).addSkill(o.skills.blocking)
        }
        verifyStrength
    }
    def removeOrganism (o: Organism) = { // organism exits the tile
        if (o != null) {
            organisms(o.asBinary).remove(o)
            strength(o.asIndex) -= o.strength
            blocking(o.asBinary).removeSkill(o.skills.blocking)
        }
        verifyStrength
    }
    def kill (o: Organism) = { // organism is dead
        removeOrganism(o)
        room.body.organisms.remove(o) // also remove from global index
    }
    def verifyStrength {
        for (idx <- 0 to nbPlayers) {
            // For unknown reasons strength sometimes gets out of sync
            if (strength(idx) < 0 || (strength(idx) > 0 && organisms(idx).size == 0)) {
                strength(idx) = organisms(idx).map(_.strength).sum
            }
        }
    }

    // Vector addition on positions may fail if the resulting position
    // is outside of the grid
    def tryAdd (i: Direction): Pos = {
        val dpos = Direction.toTuple(i)
        val newJ = this.j + dpos._1
        val newI = this.i + dpos._2
        if (room.isValid(newI, newJ)) {
            room.locs(newI, newJ)
        } else null
    }
    def jump (vert: Int, horiz: Int) : Pos = {
        val newJ = this.j + horiz
        val newI = this.i + vert
        if (room.isValid(newI, newJ)) {
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
    val maxLivingOrgs = 500
    def setSpawner (idx: Int, s: PhysicalSpawner) { spawner(idx) = s; s.position = this }
    def trySpawn (nbAlive: Int) {
        for (sp <- spawner) {
            if (sp != null) sp.step
        }
    }
    def forceSpawn {
        for (sp <- spawner) {
            if (sp != null) sp.step
        }
    }

    def battle {
        // For now, battles are nondeterministic.
        // Each organism in turn picks an ennemy that is still alive and tries to attack it.
        // This attack may fail due to skills.
        var orgs: Buffer[Organism] = Buffer()
        var split: Array[Buffer[Organism]] = Array.fill(2) { Buffer() }
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
            val idx = x.asBinary
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

    def listContents: String = { // show all organisms on the tile
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
            items.foreach(i => {s += "  Item " +  i + "\n"})
        }
        if (organisms(0).size + organisms(1).size == 0) {
            s += "  empty\n"
        }
        s
    }
}

// Aggregate functions for positions
class Grid (room: Room, rows: Int, cols: Int, nbPlayers: Int) {
    val elem = IndexedSeq.tabulate(rows, cols) { (i, j) => new Pos(room, i, j, nbPlayers) }
    def map[U] (f: Pos => U) = elem.map(_.map(f(_)))
    def filter (f: Pos => Boolean) = elem.flatten.filter(f(_))
    def apply (i: Int, j: Int) = elem(i)(j)
}
