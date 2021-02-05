import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.Font
import java.lang.System
import java.util.Random
import event._

abstract class Spawner (
    stats: StatSetGen,
    skills: SkillSetGen,
) {
    def spawn: Organism
}

class VirusSpawner (
    stats: StatSetGen,
    skills: SkillSetGen,
) extends Spawner(stats, skills) {
    def spawn: Virus = {
        new Virus(stats.instantiate, skills.instantiate)
    }
}

class CellSpawner (
    stats: StatSetGen,
    skills: SkillSetGen,
    name: String,
) extends Spawner(stats, skills) {
    def spawn: Cell = {
        new Cell(stats.instantiate, skills.instantiate, name)
    }
}
