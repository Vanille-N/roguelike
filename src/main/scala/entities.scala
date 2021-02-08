import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.Font
import java.lang.System
import scala.util.control._
import event._
import Direction._
import Behavior._

abstract class Organism (
    val stats: StatSet,
    val skills: SkillSet,
) {
    var position: Pos = null
    def isFriendly: Boolean = false
    def name: String

    def placeOnMap (p: Pos) {
        position = p
        p.addOrganism(this)
    }

    var strength: Int = 0

    // Adjust the strength according to the health of the Organism
    def updateStrength: Int = {
        val oldStrength = strength
        val healthCoeff = 20
        val powerCoeff= 30
        val speedCoeff = 5
        val resistanceCoeff = 10
        val blockingBonus = 6
        val penetrationBonus = 5
        val powerBonus = 9
        val immunityBonus = 10
        strength = ((
            stats.health.current * healthCoeff
            + stats.power.current * powerCoeff
            + stats.speed.current * speedCoeff
            + stats.resistance.current * resistanceCoeff
        ) / (healthCoeff + powerCoeff + speedCoeff + resistanceCoeff)
        + powerBonus * skills.power.get
        + blockingBonus * skills.blocking.get
        + immunityBonus * skills.immunity.get
        + penetrationBonus * skills.penetration.get
        ).toInt
        strength - oldStrength
    }

    def moveIsAllowed (room: Room, dir: Direction): Pos = {
        if (dir == STAY) return position
        val newPosition = position.tryAdd(dir)
        if (newPosition == null) return null
        val idx = if (isFriendly) 1 else 0
        if (room.locs(newPosition.i, newPosition.j).blocking(1 - idx).level <= skills.penetration.get
         && room.locs(newPosition.i, newPosition.j).blocking(idx).level < 5) {
            newPosition
        } else null
    }

    def moveTo (pos: Pos) {
        if (pos != null) {
            position.removeOrganism(this)
            placeOnMap(pos)
        }
    }

    def maybeMove (room: Room, dir: Direction): Pos = {
        if (Rng.choice(stats.decisiveness.current / 100)) return null
        moveIsAllowed(room, dir)
    }

    def attackedBy (ennemy: Organism) {
        if (ennemy.stats.power.residual > 0) {
            println(this, "attacked by", ennemy)
            if (this.skills.immunity.get <= ennemy.skills.power.get) {
                this.stats.health.residual -=
                    Rng.uniform(5, 10) * ennemy.stats.power.residual / this.stats.resistance.residual
                this.stats.speed.residual = 0 // can't move until end of turn if you were attacked
                ennemy.stats.speed.residual = 0 // ennemy has to stop to attack you
                ennemy.stats.power.residual = 0 // ennemy can only attack once in each turn
            }
        }
    }

    override def toString: String = {
        val s = skills.toString
        name + "   STR:" + strength + (if (s == "") "" else "   (" + s + ")") + "\n" +
        "      [ HP:" + stats.health.current + " | ATK:" + stats.power.current + " | DEF:" + stats.resistance.current + " | SPD:" + stats.speed.current + " | DEC:" + stats.decisiveness.current + " ]"
    }

    def focus: Pos
    def behavior: Behavior

    def step (room: Room): Boolean = { // boolean indicates if the organism can still move
        val options = PathFinder.next(this.position, this.focus, this.behavior)
        var mv: Pos = null
        var k = 0
        while (k < options.size && mv == null) {
            mv = maybeMove(room, options(k))
            k += 1
        }
        stats.speed.residual -= Rng.uniform(0, 100)
        if (stats.speed.residual <= 0) return false // can't move anymore
        if (mv != null) moveTo(mv)
        true
    }

    def sync {
        // residual health becomes actual health, other stats are restored
        stats.health.current = stats.health.residual
        stats.syncCurrent
        if (stats.health.current <= 0) {
            position.kill(this)
        } else {
            val idx = if (isFriendly) 1 else 0
            position.strength(idx) += this.updateStrength
        }
    }
}

class Virus (
    stats: StatSet,
    skills: SkillSet,
) extends Organism (stats, skills) {
    override def isFriendly = true
    var name = "virus"

    def focus: Pos = {
        this.position.room.castle.player.position
    }
    val behavior: Behavior = SEEK
}

class Cell (
    stats: StatSet,
    skills: SkillSet,
    val name: String,
    val behavior: Behavior = FLEE
) extends Organism (stats, skills) {
    def focus: Pos = {
        this.position.room.castle.player.position
    }
}
