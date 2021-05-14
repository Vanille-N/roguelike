import collection.mutable.Buffer

import swing._
import event._

import Rng.Distribution
import MakeItem.MakeItem

/* Spawners: organism creation
 * - abstract definition
 * - instances of cells
 */

case class NewOrganism (o: Organism) extends Event
case class DyingOrganism (o: Organism) extends Event

abstract class Spawner (
    var stats: StatSetGen,
    var skills: SkillSetGen,
) {
    def generate: Organism
    def itemDrop: Distribution[MakeItem] = Buffer()
    def spawn (p: Pos) = {
        val o = generate
        o.placeOnMap(p)
        p.room.addOrganism(o, p)
    }
}

// Why the _ before _stats, you ask ?
// because if it is removed there is a name collision with
// Spawner.stats, and the argument wins over the field even when
// this._ is specified.
class VirusSpawner (
    player: Player,
    _stats: StatSetGen,
    _skills: SkillSetGen = new SkillSetGen(),
) extends Spawner(_stats, _skills) {
    def generate: Virus = {
        new Virus(stats.instantiate, skills.instantiate, itemDrop, player)
    }
}

class CellSpawner (
    _stats: StatSetGen,
    _skills: SkillSetGen = new SkillSetGen(),
    name: String,
    behavior: Behavior.Behavior = Behavior.FLEE,
) extends Spawner(_stats, _skills) {
    def generate: Cell = {
        new Cell(stats.instantiate, skills.instantiate, name, behavior, itemDrop)
    }
}

class DefaultVirusSpawner (player: Player)
extends VirusSpawner(
    player,
    _stats = new StatSetGen(
        speed = new StatGen(70, 2),
        health = new StatGen(20, 2),
        power = new StatGen(30, 1),
        resistance = new StatGen(15, 1),
        decisiveness = new StatGen(70, 5),
    ),
) {}

class DefaultWallCellSpawner extends CellSpawner(
    _stats = new StatSetGen(
        speed = new StatGen(0, 0),
        health = new StatGen(100, 0),
        power = new StatGen(0, 0),
        resistance = new StatGen(100, 0),
        decisiveness = new StatGen(100, 0),
    ),
    _skills = new SkillSetGen(
        blocking = new SkillGen(5),
        immunity = new SkillGen(5),
    ),
    name = "wall cell",
) {}

class DefaultRedCellSpawner extends CellSpawner(
    _stats = new StatSetGen(
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
        (0.05, JAVEL),
        (0.1, MEMBRANE),
        (0.05, ALCOHOL),
        (0.05, KEY),
    )
}

class DefaultWhiteCellSpawner extends CellSpawner(
    _stats = new StatSetGen(
        speed = new StatGen(40, 2),
        health = new StatGen(10, 5),
        power = new StatGen(10, 2),
        resistance = new StatGen(10, 1),
        decisiveness = new StatGen(40, 10),
    ),
    _skills = new SkillSetGen(
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

class DefaultNeuronSpawner extends CellSpawner(
    _stats = new StatSetGen(
        speed = new StatGen(5, 1),
        health = new StatGen(500, 50),
        power = new StatGen(5, 1),
        resistance = new StatGen(100, 10),
        decisiveness = new StatGen(100, 10),
    ),
    name = "neuron",
) {
    import MakeItem._
    override def itemDrop = Buffer(
        (0.5, SPIKE),
        (0.3, MEMBRANE),
        (0.2, KEY),
    )
}

class DefaultPhagocytosisSpawner extends CellSpawner(
    _stats = new StatSetGen(
        speed = new StatGen(5, 1),
        health = new StatGen(30, 10),
        power = new StatGen(100, 1),
        resistance = new StatGen(20, 10),
        decisiveness = new StatGen(100, 10),
    ),
    name = "phagocytosis",
    behavior = Behavior.SEEK
) {
    import MakeItem._
    override def itemDrop = Buffer(
        (0.2, SPIKE),
        (0.2, MEMBRANE),
        (0.1, KEY),
        (0.1, HEAT),
        (0.05, KNIFE),
        (0.05, ALCOHOL),
        (0.2, NONE),
    )
}

class DefaultLymphocyteSpawner extends CellSpawner(
    _stats = new StatSetGen(
        speed = new StatGen(100, 1),
        health = new StatGen(20, 10),
        power = new StatGen(10, 1),
        resistance = new StatGen(5, 10),
        decisiveness = new StatGen(100, 10),
    ),
    name = "lymphocyte",
) {
    import MakeItem._
    override def itemDrop = Buffer(
        (0.1, KNIFE),
        (0.1, ALCOHOL),
        (0.8, NONE),
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
