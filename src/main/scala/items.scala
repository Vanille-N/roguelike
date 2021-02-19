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

abstract class Item (var position: Pos) {
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
        if(owner != null) position = owner.position
        owner = null
        pickable = true
    }

    // Item usage-related elements
    var cost_factor: Int = 0 // The real cost is cost_factor * level
    var level: Int = 1
    val max_lvl: Int = 5
    var cost_type: StatType = NONE
    var cost: Int = 0

    def updateCost: Unit = { cost = cost_factor * level }

    def isUsable (o: Organism): Boolean = {
        updateCost
        def is_enough (x: Stat) : Boolean = {
            x.residual - cost > 0
        }
        def attr_is_enough (accessor: StatSet => Stat) : Boolean = {
            is_enough(accessor(o.stats))
        }
        cost_type match {
            case HP   => { attr_is_enough(_.health) }
            case SPD  => { attr_is_enough(_.speed) }
            case POW  => { attr_is_enough(_.power) }
            case DEF  => { attr_is_enough(_.resistance) }
            case DEC  => { attr_is_enough(_.decisiveness) }
            case ALL  => { o.stats.list.exists(is_enough(_)) }
            case ANY  => { o.stats.list.forall(is_enough(_)) }
            case NONE => { true }
        }
    }

    def unpayCost (o: Organism) {
        updateCost
        def augment (x: Stat) {
            x.residual += cost
        }
        def attr_augment (accessor: StatSet => Stat) {
            augment(accessor(o.stats))
        }
        cost_type match {
            case HP   => { attr_augment(_.health) }
            case SPD  => { attr_augment(_.speed) }
            case POW  => { attr_augment(_.power) }
            case DEF  => { attr_augment(_.resistance) }
            case DEC  => { attr_augment(_.decisiveness) }
            case ALL  => { o.stats.list.foreach(x => augment(x)) }
            case ANY  => { augment(o.stats.list(Rng.uniform(0, 4))) }
            case NONE => {}
        }
    }
    def payCost (o: Organism) {
        updateCost
        def reduce (x: Stat) {
            x.residual -= cost
        }
        def attr_reduce (accessor: StatSet => Stat) {
            reduce(accessor(o.stats))
        }
        cost_type match {
            case HP   => { attr_reduce(_.health) }
            case SPD  => { attr_reduce(_.speed) }
            case POW  => { attr_reduce(_.power) }
            case DEF  => { attr_reduce(_.resistance) }
            case DEC  => { attr_reduce(_.decisiveness) }
            case ALL  => { o.stats.list.foreach(x => reduce(x)) }
            case ANY  => { reduce(o.stats.list(Rng.uniform(0, 4))) }
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
        def apply_damage (x: Stat) {
            x.residual -= damage
        }
        def attr_apply_damage (accessor: StatSet => Stat) {
            apply_damage(accessor(o.stats))
        }
        cost_type match {
            case HP   => { attr_apply_damage(_.health) }
            case SPD  => { attr_apply_damage(_.speed) }
            case POW  => { attr_apply_damage(_.power) }
            case DEF  => { attr_apply_damage(_.resistance) }
            case DEC  => { attr_apply_damage(_.decisiveness) }
            case ALL  => { o.stats.list.foreach(x => apply_damage(x)) }
            case ANY  => { apply_damage(o.stats.list(Rng.uniform(0, 4))) }
            case NONE => {}
        }
        drop
    }

    def superAction (o: Organism): Unit = {}

    def use(o: Organism, t: Organism) = { if(isUsable(o)) { action(o, t) } }

    def levelUp: Unit = { level += 1 }
    def levelDown: Unit = { level -= 1 }

    def setPos (p: Pos) = {
        position = p
        p.setItem(this)
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

object MakeItem extends Enumeration {
    type MakeItem = Value
    val KNIFE = Value
    val ALCOHOL = Value
    val NONE = Value

    def build_item (it: MakeItem, pos: Pos) : Item = {
        var item = it match {
            case KNIFE => new Knife(pos)
            case ALCOHOL => new Alcohol(pos)
            case NONE => null
        }
        item
    }
}

// Partition the Items according to their application area:
// Action on local area + straight movement
abstract class SpatialActionItem (pos: Pos) extends Item(pos) {
    var mv_vert: Int = 1
    var mv_horiz: Int = 1

    var radius: Int = 0
    def setRadius (r: Int): Unit = { radius = r }

    def LocsPicking: ListBuffer[Pos] = {
        var lst = ListBuffer[Pos]()
        for (i <- position.i - radius to position.i + radius) {
            for (j <- position.j - radius to position.j + radius) {
                if(pow(position.i - i, 2) + pow(position.j - j, 2) <= radius) { lst += position.room.locs(i, j) }
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
        position = position.jump(mv_vert, mv_horiz)
    }
}

// Action on every ( spawner | cell | virus | organism ) of the map, limited nb of uses
abstract class GlobalActionItem (pos: Pos) extends Item (pos) {
    override def action (o: Organism, t: Organism): Unit = {
        for (o <- position.room.body.organisms ) { super.action (owner, o) }
        drop
    }
}



/* --- * SpatialActionItem * ---*/
// Affaiblie tout sur son passage
class Alcohol (pos: Pos) extends SpatialActionItem(pos) {
    setPos(position)

    cost_type = HP
    cost_factor = 10
    damage_factor = 20
    targetStat = HP
}

// Tue tous les organismes et altère les spawners
class Knife (pos: Pos) extends SpatialActionItem(pos) {
    setRadius(3)
    setPos(position)

    pickable = false

    override def step: Unit = {
        for (l <- LocsPicking) {
            for (orga <- l.organisms.toList) {
                for (o <- orga.toList) { o.stats.health.residual = (0) }
            }
        }
        super.step
        position = position.jump(mv_vert, mv_horiz)
    }
}



/* --- * GlobalActionItem * ---*/
// Déplace tous les organismes aléatoirement et détériore les spawners
class BodyMovement (pos: Pos) extends GlobalActionItem(pos) {
    setPos(position)

    cost_type = HP
    cost_factor = 10
    damage_factor = 5
    targetStat = HP

    override def action (o: Organism, t: Organism): Unit = {
        for(org <- position.room.body.organisms) {
            unpayCost(owner)
            super.action(owner, org)
            for(i <- 1 to 10) {
                org.maybeMove (position.room, Direction.UP)
                org.maybeMove (position.room, Direction.DOWN)
                org.maybeMove (position.room, Direction.LEFT)
                org.maybeMove (position.room, Direction.RIGHT)
            }
        }
    }
}

// Tue tous les organisms, et détériore les spawners dès que ramassée
class Javel (pos: Pos) extends GlobalActionItem(pos) {
    setPos(position)

    override def action (o: Organism, t: Organism): Unit = {
        for(org <- position.room.body.organisms) {
            org.stats.health.residual = 0
        }
        drop
    }
}

// Améliore les spawners + ralentit les cellules;
class Heat (pos: Pos) extends GlobalActionItem(pos) {
    setPos(position)

    cost_type = HP
    cost_factor = 10
    damage_factor = -5
    targetStat = SPD
}



/* --- * LocalActionItem * --- */
// Améliore les cellules, renforce les virus (ils peuvent se faire passer pour des gentils maintenant => immUnité) ;; newhealth.residual = health.residual_factor * level + health.residual
class MembraneReplacement (pos: Pos) extends Item (pos) {
    setPos(position)

    cost_type = SPD
    cost_factor = 10
    targetStat = HP
    damage_factor = 20
}

// Renforce les virus
class Spike (pos: Pos) extends Item (pos) {
    setPos(position)

    cost_type = SPD
    cost_factor = 3
    targetStat = POW
    damage_factor = 20
}

// Cell: spd++, hp--; virus: non utilisable ;; newspeed.residual = speed.residual_factor * level ++ base_speed.residual
class CytoplasmLeak (pos: Pos) extends Item (pos) {
    setPos(position)

    cost_type = HP
    cost_factor = 20
    targetStat = SPD
    damage_factor = 20
}

// vim: set expandtab tabstop=4 shiftwidth=4 :
