import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.Font
import java.lang.System
import java.util.Random
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
            stats.health.get * healthCoeff
            + stats.power.get * powerCoeff
            + stats.speed.get * speedCoeff
            + stats.resistance.get * resistanceCoeff
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
        if (room.locs(newPosition.y, newPosition.x).blocking(1 - idx).level <= skills.penetration.get
         && room.locs(newPosition.y, newPosition.x).blocking(idx).level < 5) {
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
        val r = new Random
        if (r.nextInt(100) > stats.decisiveness.get) return null
        moveIsAllowed(room, dir)
    }

    def attackedBy (ennemy: Organism) {
        if (this.skills.immunity.get <= ennemy.skills.power.get) {
            val r = new Random()
            this.stats.health.update(-(r.nextInt(5) + 5) * ennemy.stats.power.get / this.stats.resistance.get)
        }
    }

    override def toString: String = {
        val s = skills.toString
        name + "   STR:" + strength + (if (s == "") "" else "   (" + s + ")") + "\n" +
        "      [ HP:" + stats.health.get + " | ATK:" + stats.power.get + " | DEF:" + stats.resistance.get + " | SPD:" + stats.speed.get + " | DEC:" + stats.decisiveness.get + " ]"
    }

    def focus: Pos
    def behavior: Behavior

    def step (room: Room) {
        val options = PathFinder.next(this.position, this.focus, this.behavior)
        var mv: Pos = null
        var i = 0
        while (i < options.size && mv == null) {
            mv = maybeMove(room, options(i))
            i += 1
        }
        val r = new Random
        if (r.nextInt(100) > stats.speed.get) moveTo(mv)
    }
}

class Virus (
    stats: StatSet,
    skills: SkillSet,
) extends Organism (stats, skills) {
    override def isFriendly = true
    var name = "virus"

    // stats.speed = new Stat(30, 2).instantiate
    // stats.health = new Stat(20, 2).instantiate
    // stats.power = new Stat(30, 1).instantiate
    // stats.resistance = new Stat(15, 1).instantiate
    // stats.decisiveness = new Stat(70, 5).instantiate

    def focus: Pos = {
        this.position.room.castle.player.position
    }
    def behavior: Behavior = SEEK
}

class Cell (
    stats: StatSet,
    skills: SkillSet,
    val name: String,
) extends Organism (stats, skills) {
    def focus: Pos = {
        this.position.room.castle.player.position
    }
    def behavior: Behavior = FLEE
}
