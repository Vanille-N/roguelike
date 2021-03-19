import scala.collection.mutable.Buffer
import scala.collection.mutable.Set
import scala.swing._
import event._

import Direction._
import Behavior._
import Rng.Distribution
import MakeItem._
import StatType._


/* Entities: all living things
 * - interaction with other entities (attack)
 * - movement utilities
 * - interaction with items (pickup, drop)
 */

case class OrganismDeath(o: Organism, p: Pos) extends Event

abstract class Organism (
    val stats: StatSet,
    val skills: SkillSet,
    val itemDrop: Distribution[MakeItem] = Buffer(), // Probability distribution over item drops upon death
) extends Reactor with Publisher  {
    var position: Pos = null
    def isFriendly: Boolean = false
    def name: String

    var items: Set[Item] = Set() // held by organism

    def placeOnMap (p: Pos) {
        position = p
        p.addOrganism(this)
    }

    var strength: Int = 0 // arbitrary measure of strength

    // Adjust the strength according to the health of the Organism
    def updateStrength: Int = {
        val oldStrength = strength
        // these coefficients are arbitrary
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

    // Check that
    // - the organism is allowed to move
    // - the new position is available (not blocked by a skill)
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

    // Actually move
    def moveTo (pos: Pos) {
        if (pos != null) {
            position.removeOrganism(this)
            placeOnMap(pos)
        }
    }

    // Determine new position with a probability of moving
    def maybeMove (room: Room, dir: Direction): Pos = {
        if (Rng.choice(stats.decisiveness.current / 100.0)) return null
        moveIsAllowed(room, dir)
    }

    // What to do when attacked
    def attackedBy (ennemy: Organism) {
        if (ennemy.stats.power.residual > 0) {
            /**DEBUG println(this, "attacked by", ennemy) OVER**/
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
        val sk = skills.toString // show skills
        val st = "   STR:" + strength + (if (sk == "") "" else "   (" + sk + ")") + "\n" +
        "      [ HP:" + stats.health.current + " | ATK:" + stats.power.current + " | DEF:" + stats.resistance.current + " | SPD:" + stats.speed.current + " | DEC:" + stats.decisiveness.current + " ]" // show stats
        val hold = if (items.size > 0) "\nholding {" + items.map(_.toString).mkString(",") + "}" else "" // show items
        name + st + hold
    }

    // Pathfinding parameters
    var behavior: () => Tuple2[Pos, Behavior] = null

    def step (room: Room): Boolean = { // boolean indicates if the organism can still move
        val behaviorCurrent = behavior()
        val options = room.pathFinder.next(this.position, behaviorCurrent._1, behaviorCurrent._2)
        val allowed = options.map(moveIsAllowed(room, _)).filter(x => x != null)
        // choose where to go: higher decisiveness leads to better decisions
        val mv = Rng.priorityChoice(allowed, stats.decisiveness.current / 100.0)
        stats.speed.residual -= Rng.uniform(0, 100)
        if (stats.speed.residual <= 0) return false // can't move anymore
        mv match {
            case None => ()
            case Some(p) => moveTo(p)
        }
        // try to pick up an item if there is one
        if (position.items.size > 0) {
            if (Rng.choice(stats.decisiveness.current / 100.0)) {
                val it = position.items.head
                if (it.pickUp(this)) {
                    items += it
                    publish(HeyPrint(s"I $this pick up the item, yay !"))
                }
            }
        }
        true
    }

    def sync {
        // residual health becomes actual health, other stats are restored
        stats.health.current = stats.health.residual
        stats.syncCurrent
        if (stats.health.current <= 0) {
            position.kill(this)
            position.room.publish(OrganismDeath(this, position))
            // died, maybe drop an item upon death
            Rng.weightedChoice(itemDrop) match {
                case None => ()
                case Some(item) => MakeItem.build_item(item, position)
            }
        } else {
            val idx = if (isFriendly) 1 else 0
            position.strength(idx) += this.updateStrength
        }
    }

    reactions += {
        case DyingItem(i: Item) => { publish(HeyPrint("Dying item:(\n")) }
        case UsedItem(i: Item, o:Organism, st: StatType) => { publish(HeyPrint("Used item:(\n")) }
        case PickedUpItem(i: Item, o: Organism) => { publish(HeyPrint("Picked up item:(\n")) }
        case _ => {}
    }
}

class Virus ( // friendly
    stats: StatSet,
    skills: SkillSet,
    itemDrop: Distribution[MakeItem] = Buffer(),
) extends Organism (stats, skills, itemDrop) {
    override def isFriendly = true
    var name = "virus"

    behavior = {() => { (this.position.room.body.player.position, SEEK) }}
}

class Cell ( // hostile
    stats: StatSet,
    skills: SkillSet,
    val name: String,
    val defaultBehavior: Behavior = FLEE,
    itemDrop: Distribution[MakeItem] = Buffer(),
) extends Organism (stats, skills, itemDrop) {
    behavior = {() => { (this.position.room.body.organismsBarycenter(1), defaultBehavior ) }}
}
