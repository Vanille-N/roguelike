import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.{ Color, Font }
import java.lang.System
import event._

object Alignment extends Enumeration {
    type Alignment = Value
    val HOSTILE, NEUTRAL, FRIENDLY = Value
}
import Alignment._

abstract class Organism {
    var position: Pos = null
    def alignment: Alignment
    def name: String
    def placeOnMap (p: Pos) {
        position = p
        p.addOrganism(alignment, this)
    }
<<<<<<< HEAD
    var stats: Stats
    var strength: Int
=======

    ///////////////////////////////////////////////////////////////
    // Movement-related code: Castle object will call move(), which
    // calls react() as a function of the actor's speed; react()
    // is supposed to contain the actor-specific code.
    // move and react return true if something "important" happens.

    var speed = 100        // speed relative to player
    var residualSpeed = 0

    def move: Boolean = {
        var retval = false
        residualSpeed += speed
        while (residualSpeed >= 100)
            { residualSpeed -= 100; retval = react || retval }
        return retval
    }

    def goUp (room : Room) : Boolean = {
	if (room.cells(this.position.y-1, this.position.x).isFree) {
		this.position.removeActor
		this.placeOnMap(room.cells(this.position.y-1, this.position.x))
		return true
	}
	else return false
    }
    def goDown (room : Room) : Boolean = {
	if (room.cells(this.position.y+1, this.position.x).isFree) {
		this.position.removeActor
		this.placeOnMap(room.cells(this.position.y+1, this.position.x))
		return true
	}
	else return false
    }
    def goRight (room : Room) : Boolean = {
	if (room.cells(this.position.y, this.position.x+1).isFree) {
		this.position.removeActor
		this.placeOnMap(room.cells(this.position.y, this.position.x+1))
		return true
	} else { return false }
    }
    def goLeft (room : Room) : Boolean = {
	if (room.cells(this.position.y, this.position.x-1).isFree) {
		this.position.removeActor
		this.placeOnMap(room.cells(this.position.y, this.position.x-1))
		return true
	} else { return false }
    }

    // by default, do nothing
    def react: Boolean = { return false }
>>>>>>> d3a005608459075c4cb7f1ccc729bc11c70cddd3
}

class Virus extends Organism {
    def alignment = FRIENDLY
    def name = "virus"
    var stats = new Stats()
    var strength = stats.toStrength
}

abstract class Cell extends Organism {
    def alignment = HOSTILE
    def name = "cell"
    var stats = new Stats()
    var strength = stats.toStrength
}

class WhiteCell extends Cell {
    override def name = "white cell"
}

class RedCell extends Cell {
    override def name = "red cell"
    override def alignment = NEUTRAL
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
