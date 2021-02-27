import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.Font
import java.lang.System
import event._

import Rng.Distribution
import MakeItem.MakeItem

/* Spawners: organism creation
 * - abstract definition
 * - instances of cells
 */

abstract class Spawner (
    stats: StatSetGen,
    skills: SkillSetGen,
) {
    def generate: Organism
    def itemDrop: Distribution[MakeItem] = Buffer()
    def spawn (p: Pos) = {
        val o = generate
        o.placeOnMap(p)
        p.room.addOrganism(o, p)
    }
}

class VirusSpawner (
    stats: StatSetGen,
    skills: SkillSetGen = new SkillSetGen(),
) extends Spawner(stats, skills) {
    def generate: Virus = {
        new Virus(stats.instantiate, skills.instantiate, itemDrop)
    }
}

class CellSpawner (
    stats: StatSetGen,
    skills: SkillSetGen = new SkillSetGen(),
    name: String,
    behavior: Behavior.Behavior = Behavior.FLEE,
) extends Spawner(stats, skills) {
    def generate: Cell = {
        new Cell(stats.instantiate, skills.instantiate, name, behavior, itemDrop)
    }
}

class DefaultVirusSpawner extends VirusSpawner(
    stats = new StatSetGen(
        speed = new StatGen(70, 2),
        health = new StatGen(20, 2),
        power = new StatGen(30, 1),
        resistance = new StatGen(15, 1),
        decisiveness = new StatGen(70, 5),
    ),
) {}

class DefaultWallCellSpawner extends CellSpawner(
    stats = new StatSetGen(
        speed = new StatGen(0, 0),
        health = new StatGen(100, 0),
        power = new StatGen(0, 0),
        resistance = new StatGen(100, 0),
        decisiveness = new StatGen(100, 0),
    ),
    skills = new SkillSetGen(
        blocking = new SkillGen(5),
        immunity = new SkillGen(5),
    ),
    name = "wall cell",
) {}

class DefaultRedCellSpawner extends CellSpawner(
    stats = new StatSetGen(
        speed = new StatGen(50, 2),
        health = new StatGen(50, 20),
        power = new StatGen(0, 0),
        resistance = new StatGen(5, 1),
        decisiveness = new StatGen(30, 5),
    ),
    name = "red cell",
) {
    import MakeItem._
    override def itemDrop = Buffer(
        (0.7, NONE),
        (0.1, JAVEL),
        (0.1, MEMBRANE),
        (0.1, ALCOHOL),
    )
}

class DefaultWhiteCellSpawner extends CellSpawner(
    stats = new StatSetGen(
        speed = new StatGen(40, 2),
        health = new StatGen(10, 5),
        power = new StatGen(10, 2),
        resistance = new StatGen(10, 1),
        decisiveness = new StatGen(40, 10),
    ),
    skills = new SkillSetGen(
        power = new SkillGen(1),
        penetration = new SkillGen(1),
    ),
    name = "red cell",
    behavior = Behavior.SEEK
) {
    import MakeItem._
    override def itemDrop = Buffer(
        (0.5, NONE),
        (0.2, SPIKE),
        (0.1, LEAK),
        (0.1, HEAT),
        (0.02, KNIFE),
    )
}

// ties a spawner to a location
class PhysicalSpawner (val model: Spawner, var threshold: Double, var pulse: Int) {
    var position: Pos = null
    def spawn {
        for (i <- 0 to pulse) model.spawn(position)
    }
    def step {
        if (position != null && Rng.choice(threshold)) spawn
    }
}
