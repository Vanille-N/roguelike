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
}

class Virus extends Organism {
    override def isFriendly = true
    def name = "virus"
}

abstract class Cell extends Organism {
    def name = "cell"
}

class WhiteCell extends Cell {
    override def name = "white cell"
}

class RedCell extends Cell {
    override def name = "red cell"
}

//
// class Hero extends Actor {
//     val symbol = new Symbol('H',Color.white)
//     override val name = "you"
//     var hasWeapon = false
//     override def isHero: Boolean = { return true }
//
//     life = 5
// }
//
// abstract class AgressiveActor extends Actor {
//     var enemy: Actor = null
//
//     def setEnemy (a: Actor) = { enemy = a }
//
//     // go after the enemy and attack if the enemy is next to the actor
//     def pursuit: Boolean = {
//         if (enemy == null) return false
//         if (enemy.life == 0) { enemy = null; return false }
//
//         val next = position.room.cells.neighbours(position).
//                     minBy(_.l2dist(enemy.position))
//
//         if (next.actor == null) {
//             next.floor.enter(this,next)
//             return false
//         }
//
//         if (next.actor == enemy) {
//             enemy.hit
//             return true
//         }
//
//         return false
//     }
// }
//
// class Fairy extends Actor {
//     val symbol = new Symbol('F', Color.green)
//     override val name = "fairy"
//
//     override def hear: Unit = {
//         position.removeActor
//     }
// }
//
// class Gnome extends AgressiveActor {
//     val symbol = new Symbol('G',Color.magenta)
//     override val name = "gnome"
//     life = 3
//     speed = 70
//
//     override def react: Boolean = { return pursuit }
// }
