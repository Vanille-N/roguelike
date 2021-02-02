import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.Font
import java.lang.System
import event._

import Direction._

abstract class Organism {
    var position: Pos = null
    def isFriendly: Boolean = false
    def name: String

    def placeOnMap (p: Pos) {
        position = p
        p.addOrganism(this)
    }

    var stats: Stats = new Stats
    var skills: Skills = new Skills
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
            stats.health * healthCoeff
            + stats.power * powerCoeff
            + stats.speed * speedCoeff
            + stats.resistance * resistanceCoeff
        ) / (healthCoeff + powerCoeff + speedCoeff + resistanceCoeff)
        + powerBonus * skills.power.level
        + blockingBonus * skills.blocking.level
        + immunityBonus * skills.immunity.level
        + penetrationBonus * skills.penetration.level
        ).toInt
        strength - oldStrength
    }

    def move (room: Room, dir: Direction): Boolean = {
        val newPosition = position.tryAdd(dir)
        val idx = if (isFriendly) 1 else 0
        if (room.locs(newPosition.y, newPosition.x).blocking(1 - idx).level < skills.penetration.level) {
            position.removeOrganism(this)
            placeOnMap(newPosition)
            true
        } else false
    }

    override def toString: String = {
        val s = skills.toString
        name + "   STR:" + strength + (if (s == "") "" else "   (" + s + ")") + "\n" +
        "      [ HP:" + stats.health + " | ATK:" + stats.power + " | DEF:" + stats.resistance + " | SPD:" + stats.speed + " ]"
    }
}

class Virus extends Organism {
    override def isFriendly = true
    def name = "virus"

    stats.speed = new Stat(30, 2).instantiate
    stats.health = new Stat(20, 2).instantiate
    stats.power = new Stat(30, 1).instantiate
    stats.resistance = new Stat(15, 1).instantiate
}

abstract class Cell extends Organism {
    def name = "cell"
}

class WhiteCell extends Cell {
    override def name = "white cell"

    stats.speed = new Stat(10, 2).instantiate
    stats.health = new Stat(10, 5).instantiate
    stats.power = new Stat(10, 2).instantiate
    stats.resistance = new Stat(10, 1).instantiate

    skills.penetration = new Skill(1)
    skills.power = new Skill(1)
}

class RedCell extends Cell {
    override def name = "red cell"

    stats.speed = new Stat(5, 2).instantiate
    stats.health = new Stat(50, 20).instantiate
    stats.power = new Stat(0, 0).instantiate
    stats.resistance = new Stat(5, 1).instantiate
}

class WallCell extends Cell {
    override def name = "wall cell"

    stats.speed = new Stat(0, 0).instantiate
    stats.health = new Stat(100, 0).instantiate
    stats.power = new Stat(0, 0).instantiate
    stats.resistance = new Stat(100, 0).instantiate

    skills.immunity = new Skill(5)
    skills.blocking = new Skill(5)
}
