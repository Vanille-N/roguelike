import scala.collection.mutable.ListBuffer
import Math._

object StatType extends Enumeration {
    type StatType = Value
    val HP   = Value("health.residual")
    val SPD  = Value("speed.residual")
    val POW  = Value("power.residual")
    val DEF  = Value("resistance.residual")
    val DEC  = Value("decisiveness.residual")
    val ALL  = Value("every field")
    val ANY  = Value("any field")
    val NONE = Value("free to use")
}

import StatType._

abstract class Item {
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
            case HP   => { return (o.stats.health.residual - cost > 0)       }
            case SPD  => { return (o.stats.speed.residual - cost > 0)        }
            case POW  => { return (o.stats.power.residual - cost > 0)        }
            case DEF  => { return (o.stats.resistance.residual - cost > 0)   }
            case DEC  => { return (o.stats.decisiveness.residual - cost > 0) }
            case ALL  => { return ( (o.stats.health.residual - cost > 0)
                        && (o.stats.speed.residual - cost > 0)
                        && (o.stats.power.residual - cost > 0)
                        && (o.stats.resistance.residual - cost > 0)
                        && (o.stats.decisiveness.residual - cost > 0) ) }
            case ANY  => { return ( (o.stats.health.residual - cost > 0)
                        || (o.stats.speed.residual - cost > 0)
                        || (o.stats.power.residual - cost > 0)
                        || (o.stats.resistance.residual - cost > 0)
                        || (o.stats.decisiveness.residual - cost > 0) ) }
            case NONE => { return true }
        }
    }

    def unpayCost (o: Organism): Unit ={
        updateCost
        cost_type match {
            case HP   => { o.stats.health.residual = (o.stats.health.residual + cost)             }
            case SPD  => { o.stats.speed.residual = (o.stats.speed.residual + cost)               }
            case POW  => { o.stats.power.residual = (o.stats.power.residual + cost)               }
            case DEF  => { o.stats.resistance.residual = (o.stats.resistance.residual + cost)     }
            case DEC  => { o.stats.decisiveness.residual = (o.stats.decisiveness.residual + cost) }
            case ALL  => { o.stats.health.residual = (o.stats.health.residual + cost)
                        o.stats.speed.residual = (o.stats.speed.residual + cost)
                        o.stats.power.residual = (o.stats.power.residual + cost)
                        o.stats.resistance.residual = (o.stats.resistance.residual + cost)
                        o.stats.decisiveness.residual = (o.stats.decisiveness.residual + cost) }
            case ANY  => {
                Rng.uniform(0, 4) match {
                    case 0 => o.stats.health.residual = (o.stats.health.residual + cost)
                    case 1 => o.stats.speed.residual = (o.stats.speed.residual + cost)
                    case 2 => o.stats.power.residual = (o.stats.power.residual + cost)
                    case 3 => o.stats.resistance.residual = (o.stats.resistance.residual + cost)
                    case 4 => o.stats.decisiveness.residual = (o.stats.decisiveness.residual + cost)
                }
            }
            case NONE => {}
        }
    }
    def payCost (o: Organism): Unit ={
        updateCost
        cost_type match {
            case HP   => { o.stats.health.residual = (o.stats.health.residual - cost)             }
            case SPD  => { o.stats.speed.residual = (o.stats.speed.residual - cost)               }
            case POW  => { o.stats.power.residual = (o.stats.power.residual - cost)               }
            case DEF  => { o.stats.resistance.residual = (o.stats.resistance.residual - cost)     }
            case DEC  => { o.stats.decisiveness.residual = (o.stats.decisiveness.residual - cost) }
            case ALL  => { o.stats.health.residual = (o.stats.health.residual - cost)
                        o.stats.speed.residual = (o.stats.speed.residual - cost)
                        o.stats.power.residual = (o.stats.power.residual - cost)
                        o.stats.resistance.residual = (o.stats.resistance.residual - cost)
                        o.stats.decisiveness.residual = (o.stats.decisiveness.residual - cost) }
            case ANY  => {
                Rng.uniform(0, 4) match {
                    case 0 => o.stats.health.residual = (o.stats.health.residual - cost)
                    case 1 => o.stats.speed.residual = (o.stats.speed.residual - cost)
                    case 2 => o.stats.power.residual = (o.stats.power.residual - cost)
                    case 3 => o.stats.resistance.residual = (o.stats.resistance.residual - cost)
                    case 4 => o.stats.decisiveness.residual = (o.stats.decisiveness.residual - cost)
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
            case HP   => { t.stats.health.residual = (t.stats.health.residual - damage)             }
            case SPD  => { t.stats.speed.residual = (t.stats.speed.residual - damage)               }
            case POW  => { t.stats.power.residual = (t.stats.power.residual - damage)               }
            case DEF  => { t.stats.resistance.residual = (t.stats.resistance.residual - damage)     }
            case DEC  => { t.stats.decisiveness.residual = (t.stats.decisiveness.residual - damage) }
            case ALL  => {
                t.stats.health.residual = (t.stats.health.residual - damage)
                t.stats.speed.residual = (t.stats.speed.residual - damage)
                t.stats.power.residual = (t.stats.power.residual - damage)
                t.stats.resistance.residual = (t.stats.resistance.residual - damage)
                t.stats.decisiveness.residual = (t.stats.decisiveness.residual - damage)
            }
            case ANY  => {
                Rng.uniform(0, 4) match {
                    case 0 => t.stats.health.residual = (t.stats.health.residual - damage)
                    case 1 => t.stats.speed.residual = (t.stats.speed.residual - damage)
                    case 2 => t.stats.power.residual = (t.stats.power.residual - damage)
                    case 3 => t.stats.resistance.residual = (t.stats.resistance.residual - damage)
                    case 4 => t.stats.decisiveness.residual = (t.stats.decisiveness.residual - damage)
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
        if (owner != null && isUsable(owner)
        && Rng.choice(level / max_lvl)
        && Rng.choice(owner.stats.decisiveness.residual / 100)) {
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
                    for (o <- orga.toList) { if(o != owner) { o.stats.health.residual = (o.stats.health.residual - damage) } }
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
                for (o <- orga.toList) { o.stats.health.residual = (0) }
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
            org.stats.health.residual = 0
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
// Améliore les cellules, renforce les virus (ils peuvent se faire passer pour des gentils maintenant => immUnité) ;; newhealth.residual = health.residual_factor * level + health.residual
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

// Cell: spd++, hp--; virus: non utilisable ;; newspeed.residual = speed.residual_factor * level ++ base_speed.residual
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
