import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.{ Color, Font }
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

    def calculateStrength: Int = {
        10 // bogus
    }

    var stats: Stats = new Stats
    var skills: Skills = new Skills
    var strength: Int = calculateStrength

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
        name + "[STR:" + strength + "; HP:" + stats.health.amount + "; ATK:" + stats.strength.amount + "]"
    }
}

class Virus extends Organism {
    override def isFriendly = true
    def name = "virus"

    stats.speed = new Stat(30, 2)
    stats.health = new Stat(20, 2)
    stats.strength = new Stat(30, 1)
    stats.resistance = new Stat(15, 1)
}

abstract class Cell extends Organism {
    def name = "cell"
}

class WhiteCell extends Cell {
    override def name = "white cell"

    stats.speed = new Stat(10, 2)
    stats.health = new Stat(10, 5)
    stats.strength = new Stat(10, 2)
    stats.resistance = new Stat(10, 1)

    skills.penetration = new Skill(1)
    skills.power = new Skill(1)
}

class RedCell extends Cell {
    override def name = "red cell"

    stats.speed = new Stat(5, 2)
    stats.health = new Stat(50, 20)
    stats.strength = new Stat(0, 0)
    stats.resistance = new Stat(5, 1)
}

class WallCell extends Cell {
    override def name = "wall cell"

    stats.speed = new Stat(0, 0)
    stats.health = new Stat(100, 0)
    stats.strength = new Stat(0, 0)
    stats.resistance = new Stat(100, 0)

    skills.immunity = new Skill(5)
    skills.blocking = new Skill(5)
}
