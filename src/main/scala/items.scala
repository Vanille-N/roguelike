import scala.collection.mutable.ListBuffer
import java.util.Random
import Math._

object StatType extends Enumeration {
    type StatType = Value
    val HP   = Value("health")
    val SPD  = Value("speed")
    val POW  = Value("power")
    val DEF  = Value("resistance")
    val DEC  = Value("decisiveness")
    val ALL  = Value("every field")
    val ANY  = Value("any field")
    val NONE = Value("free to use")
}

import StatType._

abstract class Item {
    val r = new Random()
    var room: Room = null
    var castle: Castle = null

    // Item pickink up -- dropping
    var pickable: Boolean = true
    var owner: Organism = null

    def pickUp (o: Organism): Unit = {
        if(pickable) {
            owner = o
            pickable = false
        }
    }
    def drop: Unit = {
        owner = null
        pickable = true
    }

    // Item usage-related elements
    var cost_factor: Int = 0// The real cost is cost_factor * level
    var level: Int = 1
    val max_lvl: Int = 5
    var cost_type: StatType = NONE
    var cost: Int = 0

    def updateCost: Unit = { cost = cost_factor * level }

    def isUsable (o: Organism): Boolean = {
        updateCost
        cost_type match {
            case HP   => { return (o.stats.health.get - cost > 0)       }
            case SPD  => { return (o.stats.speed.get - cost > 0)        }
            case POW  => { return (o.stats.power.get - cost > 0)        }
            case DEF  => { return (o.stats.resistance.get - cost > 0)   }
            case DEC  => { return (o.stats.decisiveness.get - cost > 0) }
            case ALL  => { return ( (o.stats.health.get - cost > 0)
                        && (o.stats.speed.get - cost > 0)
                        && (o.stats.power.get - cost > 0)
                        && (o.stats.resistance.get - cost > 0)
                        && (o.stats.decisiveness.get - cost > 0) ) }
            case ANY  => { return ( (o.stats.health.get - cost > 0)
                        || (o.stats.speed.get - cost > 0)
                        || (o.stats.power.get - cost > 0)
                        || (o.stats.resistance.get - cost > 0)
                        || (o.stats.decisiveness.get - cost > 0) ) }
            case NONE => { return true }
        }
    }

    def unpayCost (o: Organism): Unit ={
        updateCost
        cost_type match {
            case HP   => { o.stats.health.set(o.stats.health.get + cost)             }
            case SPD  => { o.stats.speed.set(o.stats.speed.get + cost)               }
            case POW  => { o.stats.power.set(o.stats.power.get + cost)               }
            case DEF  => { o.stats.resistance.set(o.stats.resistance.get + cost)     }
            case DEC  => { o.stats.decisiveness.set(o.stats.decisiveness.get + cost) }
            case ALL  => { o.stats.health.set(o.stats.health.get + cost)
                        o.stats.speed.set(o.stats.speed.get + cost)
                        o.stats.power.set(o.stats.power.get + cost)
                        o.stats.resistance.set(o.stats.resistance.get + cost)
                        o.stats.decisiveness.set(o.stats.decisiveness.get + cost) }
            case ANY  => {
                r.nextInt(4) match {
                    case 0 => o.stats.health.set(o.stats.health.get + cost)
                    case 1 => o.stats.speed.set(o.stats.speed.get + cost)
                    case 2 => o.stats.power.set(o.stats.power.get + cost)
                    case 3 => o.stats.resistance.set(o.stats.resistance.get + cost)
                    case 4 => o.stats.decisiveness.set(o.stats.decisiveness.get + cost)
                }
            }
            case NONE => {}
        }
    }
    def payCost (o: Organism): Unit ={
        updateCost
        cost_type match {
            case HP   => { o.stats.health.set(o.stats.health.get - cost)             }
            case SPD  => { o.stats.speed.set(o.stats.speed.get - cost)               }
            case POW  => { o.stats.power.set(o.stats.power.get - cost)               }
            case DEF  => { o.stats.resistance.set(o.stats.resistance.get - cost)     }
            case DEC  => { o.stats.decisiveness.set(o.stats.decisiveness.get - cost) }
            case ALL  => { o.stats.health.set(o.stats.health.get - cost)
                        o.stats.speed.set(o.stats.speed.get - cost)
                        o.stats.power.set(o.stats.power.get - cost)
                        o.stats.resistance.set(o.stats.resistance.get - cost)
                        o.stats.decisiveness.set(o.stats.decisiveness.get - cost) }
            case ANY  => {
                r.nextInt(4) match {
                    case 0 => o.stats.health.set(o.stats.health.get - cost)
                    case 1 => o.stats.speed.set(o.stats.speed.get - cost)
                    case 2 => o.stats.power.set(o.stats.power.get - cost)
                    case 3 => o.stats.resistance.set(o.stats.resistance.get - cost)
                    case 4 => o.stats.decisiveness.set(o.stats.decisiveness.get - cost)
                }
            }
            case NONE => {}
        }
    }

    var damage: Int = 0
    var damage_factor: Int = 1
    def damageUpdate: Unit = { damage = damage_factor * level }

    var targetStat: StatType = NONE
    def action (o: Organism, t: Organism): Unit = {
        payCost(o)
        damageUpdate
        targetStat match {
            case HP   => { t.stats.health.set(t.stats.health.get - damage)             }
            case SPD  => { t.stats.speed.set(t.stats.speed.get - damage)               }
            case POW  => { t.stats.power.set(t.stats.power.get - damage)               }
            case DEF  => { t.stats.resistance.set(t.stats.resistance.get - damage)     }
            case DEC  => { t.stats.decisiveness.set(t.stats.decisiveness.get - damage) }
            case ALL  => {
                t.stats.health.set(t.stats.health.get - damage)
                t.stats.speed.set(t.stats.speed.get - damage)
                t.stats.power.set(t.stats.power.get - damage)
                t.stats.resistance.set(t.stats.resistance.get - damage)
                t.stats.decisiveness.set(t.stats.decisiveness.get - damage)
            }
            case ANY  => {
                r.nextInt(4) match {
                    case 0 => t.stats.health.set(t.stats.health.get - damage)
                    case 1 => t.stats.speed.set(t.stats.speed.get - damage)
                    case 2 => t.stats.power.set(t.stats.power.get - damage)
                    case 3 => t.stats.resistance.set(t.stats.resistance.get - damage)
                    case 4 => t.stats.decisiveness.set(t.stats.decisiveness.get - damage)
                }
            }
            case NONE => {}
        }
        drop
    }
    
    def superAction (o: Organism): Unit = {}

    def use(o: Organism, t: Organism) = { if(isUsable(o)) { action(o, t) } }

    def levelUp: Unit = { level += 1 }
    def levelDown: Unit = { level -= 1 }

    // Position related elements
    var position: Pos = null
    def setPos(p: Pos): Unit = { position = p }
    def setRoom(r: Room): Unit = { room = r }
    def setCastle(c: Castle): Unit = { castle = c }

    def addToPosition(i: Int, j: Int) = {
        // TODO !
    }

    // Game evolution
    def step: Unit = {
        if(owner != null && isUsable(owner) && r.nextInt(max_lvl) > level && r.nextInt(100) > owner.stats.decisiveness.get) {
            use(owner, owner)
        }
    }
}



// Partition the Items according to their application area:
// Action on local area + straight movement
abstract class SpatialActionItem extends Item {
    var mvt_a: Int = 1
    var mvt_b: Int = 1

    var radius: Int = 0
    def setRadius (r: Int): Unit = { radius = r }

    def LocsPicking: ListBuffer[Pos] = {
        var lst = ListBuffer[Pos]()
        for (i <- position.i - radius to position.i + radius) {
            for (j <- position.j - radius to position.j + radius) {
                if(pow(position.i - i, 2) + pow(position.j - j, 2) <= radius) { lst += room.locs(i, j) }
            }
        }
        lst
    }

    override def drop: Unit = {
        super.drop
        pickable = false
    }

    override def action (o: Organism, t: Organism): Unit = {
        unpayCost(o)
        super.action (o, t)
    }

    override def step: Unit = {
        if(pickable == false) {
            damageUpdate
            for (l <- LocsPicking) {
                for (orga <- l.organisms.toList) {
                    for (o <- orga.toList) { if(o != owner) { o.stats.health.set(o.stats.health.get - damage) } }
                }
            }
        }
        super.step
        addToPosition(mvt_a, mvt_b)
    }
}

// Action on every ( spawner | cell | virus | organism ) of the map, limited nb of uses
abstract class GlobalActionItem extends Item {
    override def action (o: Organism, t: Organism): Unit = {
        for (o <- castle.organisms ) { super.action (owner, o) }
        drop
    }
}



/* --- * SpatialActionItem * ---*/
// Affaiblie tout sur son passage
class Alcohol (val cstle: Castle, val pos_init: Pos, val dir1: Int, val dir2: Int) extends SpatialActionItem {
    setRoom(castle.room)
    setPos(pos_init)

    cost_type = HP
    cost_factor = 10
    damage_factor = 20
    targetStat = HP

    mvt_a = dir1
    mvt_b = dir2
}

// Tue tous les organismes et altère les spawners
class Knife (val cstle: Castle, val pos_init: Pos, val dir1: Int, val dir2: Int) extends SpatialActionItem {
    setRadius(3)
    setCastle(cstle)
    setRoom(castle.room)
    setPos(pos_init)

    pickable = false

    override def step: Unit = {
        for (l <- LocsPicking) {
            for (orga <- l.organisms.toList) {
                for (o <- orga.toList) { o.stats.health.set(0) }
            }
        }
        super.step
        addToPosition(mvt_a, mvt_b)
    }

    mvt_a = dir1
    mvt_b = dir2
}



/* --- * GlobalActionItem * ---*/
// Déplace tous les organismes aléatoirement et détériore les spawners
class BodyMovement (val cstle: Castle, val pos_init: Pos) extends GlobalActionItem {
    setCastle(cstle)
    setRoom(castle.room)
    setPos(pos_init)
    
    cost_type = HP
    cost_factor = 10
    damage_factor = 5
    targetStat = HP

    override def action (o: Organism, t: Organism): Unit = {
        for(org <- castle.organisms) {
            unpayCost(owner)
            super.action(owner, org)
            for(i <- 1 to 10) {
                org.maybeMove (room, Direction.UP)
                org.maybeMove (room, Direction.DOWN)
                org.maybeMove (room, Direction.LEFT)
                org.maybeMove (room, Direction.RIGHT)
            }
        }
    }
}

// Tue tous les organisms, et détériore les spawners  dès que ramassée
class Javel (val cstle: Castle, val pos_init: Pos) extends GlobalActionItem {
    setCastle(cstle)
    setRoom(castle.room)
    setPos(pos_init)

    override def action (o: Organism, t: Organism): Unit = {
        for(org <- castle.organisms) {
            org.stats.health.set(0)
        }
        drop
    }
}

// Améliore les spawners + ralentit les cellules;
class Heat (val cstle: Castle, val pos_init: Pos) extends GlobalActionItem {
    setCastle(cstle)
    setRoom(castle.room)
    setPos(pos_init)
    
    cost_type = HP
    cost_factor = 10
    damage_factor = -5
    targetStat = SPD
}



/* --- * LocalActionItem * --- */
// Améliore les cellules, renforce les virus (ils peuvent se faire passer pour des gentils maintenant => immUnité) ;; newhealth = health_factor * level + health
class MembraneReplacement (val cstle: Castle, val pos_init: Pos) extends Item {
    setCastle(cstle)
    setRoom(castle.room)
    setPos(pos_init)
    
    cost_type = SPD
    cost_factor = 10
    targetStat = HP
    damage_factor = 20
}

// Renforce les virus
class Spike (val cstle: Castle, val pos_init: Pos) extends Item {
    setCastle(cstle)
    setRoom(castle.room)
    setPos(pos_init)
    
    cost_type = SPD
    cost_factor = 3
    targetStat = POW
    damage_factor = 20
}

// Cell: spd++, hp--; virus: non utilisable ;; newspeed = speed_factor * level ++ base_speed
class CytoplasmLeak (val cstle: Castle, val pos_init: Pos) extends Item {
    setCastle(cstle)
    setRoom(castle.room)
    setPos(pos_init)
    
    cost_type = HP
    cost_factor = 20
    targetStat = SPD
    damage_factor = 20
}

// vim: set expandtab tabstop=4 shiftwidth=4 :
