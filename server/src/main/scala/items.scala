import scala.collection.mutable.{ ListBuffer, Buffer, HashMap, Set }
import Math._

import swing._
import event._

// What stat is targeted by an item
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


/* Items interface
 * - interaction with an organism's stats
 * - pickup/drop
 * - usage
 */

/*
* The DyingItem event is published whenever an item gets destroyed.
* It must at least be listened by the body, which will relay the information.
*/
case class DyingItem (i: Item) extends Event

case class PickedUpItem (i: Item, o: Organism) extends Event // an item has been picked up
case class UsedItem(i: Item, o: Organism, st: StatType) extends Event // an item has been used over an Organism


sealed trait Owner;
case class PosOwned(p: Pos) extends Owner;
case class OrgOwned(o: Organism) extends Owner;
case class UnOwned() extends Owner;
case class PlayOwned(p: Player) extends Owner;

abstract class Item (var owner: Owner) extends Publisher {
    var pickable: Boolean = true
    setOwner(owner)
    if (position != null) {
        position.room.body.items += this
    }

    def abandonOwner {
        owner match {
            case PosOwned(position) => {
                position.room.body.deafTo(this)
                position.listenTo(this)
                position.items -= this
            }
            case OrgOwned(orga) => {
                orga.deafTo(this)
                orga.items -= this
            }
            case PlayOwned(player) => {
                player.inventory -= this
            }
            case _ => ()
        }
        owner = UnOwned()
        pickable = false
    }

    def setOwner (newOwner: Owner) {
        val former = owner
        abandonOwner
        val tmp = owner
        newOwner match {
            case PosOwned(position) => {
                position.room.body.listenTo(this)
                position.listenTo(this)
                position.items += this
                pickable = true
            }
            case OrgOwned(orga) => {
                orga.items += this
                orga.listenTo(this)
                pickable = false
            }
            case PlayOwned(player) => {
                player.inventory += this
                pickable = false
            }
            case _ => ()
        }
        owner = newOwner
    }

    def pickUp (o: Organism): Boolean = { // tile -> owner
        if (pickable) {
            o match {
                case v:Virus if v.player.itemPolicyTake => {
                    setOwner(PlayOwned(v.player))
                    true
                }
                case o:Organism => {
                    // give to organism
                    publish(PickedUpItem(this, o))
                    setOwner(OrgOwned(o))
                    true
                }
            }
        } else false
    }
    
    def position: Pos = {
        owner match {
            case OrgOwned(orga) => orga.position
            case PlayOwned(player) => player.position
            case PosOwned(position) => position
            case _ => null
        }
    }

    def drop: Unit = { // owner -> tile
        val pos = position
        if (pos != null) {
            setOwner(PosOwned(pos))
        }
    }

    def destroy { // remove from global item index
        publish(DyingItem(this))
        abandonOwner
    }

    // Item usage-related elements
    var cost_factor: Int = 0 // The real cost is cost_factor * level
    var level: Int = 1
    val max_lvl: Int = 5
    var cost_type: StatType = NONE
    var cost: Int = 0
    def sacrificeValue: Int

    def updateCost: Unit = { cost = cost_factor * level }

    def isUsable (o: Organism): Boolean = { // check if organism has enough stat points to pay the activation cost
        updateCost
        def is_enough (x: Stat) : Boolean = {
            x.residual - cost > 0
        }
        def attr_is_enough (accessor: StatSet => Stat) : Boolean = {
            is_enough(accessor(o.stats))
        }
        cost_type match {
            case HP   => attr_is_enough(_.health)
            case SPD  => attr_is_enough(_.speed)
            case POW  => attr_is_enough(_.power)
            case DEF  => attr_is_enough(_.resistance)
            case DEC  => attr_is_enough(_.decisiveness)
            case ALL  => o.stats.list.exists(is_enough(_))
            case ANY  => o.stats.list.forall(is_enough(_))
            case NONE => true
        }
    }

    def unpayCost (o: Organism) { // cancel stat reduction for activation cost
        updateCost
        def augment (x: Stat) {
            x.residual += cost
        }
        def attr_augment (accessor: StatSet => Stat) {
            augment(accessor(o.stats))
        }
        cost_type match {
            case HP   => attr_augment(_.health)
            case SPD  => attr_augment(_.speed)
            case POW  => attr_augment(_.power)
            case DEF  => attr_augment(_.resistance)
            case DEC  => attr_augment(_.decisiveness)
            case ALL  => o.stats.list.foreach(x => augment(x))
            case ANY  => augment(o.stats.list(Rng.uniform(0, 4)))
            case NONE => ()
        }
    }
    def payCost (o: Organism) { // apply stat reduction for activation cost
        updateCost
        def reduce (x: Stat) {
            x.residual -= cost
        }
        def attr_reduce (accessor: StatSet => Stat) {
            reduce(accessor(o.stats))
        }
        cost_type match {
            case HP   => o.inflictDamage(cost, ItemCostKill())
            case SPD  => attr_reduce(_.speed)
            case POW  => attr_reduce(_.power)
            case DEF  => attr_reduce(_.resistance)
            case DEC  => attr_reduce(_.decisiveness)
            case ALL  => o.stats.list.foreach(x => reduce(x))
            case ANY  => reduce(o.stats.list(Rng.uniform(0, 4)))
            case NONE => ()
        }
    }

    var damage: Int = 0
    var damage_factor: Int = 1
    def damageUpdate: Unit = { damage = damage_factor * level }

    var targetStat: StatType = NONE
    def action (o: Organism, t: Organism): Unit = { // execute the effect of the item
        if(o != null) {
            payCost(o)
            damageUpdate
            def apply_damage (x: Stat) {
                x.residual -= damage
            }
            def attr_apply_damage (accessor: StatSet => Stat) {
                apply_damage(accessor(o.stats))
            }
            cost_type match {
                case HP   => o.inflictDamage(cost, ItemCostKill())
                case SPD  => attr_apply_damage(_.speed)
                case POW  => attr_apply_damage(_.power)
                case DEF  => attr_apply_damage(_.resistance)
                case DEC  => attr_apply_damage(_.decisiveness)
                case ALL  => o.stats.list.foreach(x => apply_damage(x))
                case ANY  => apply_damage(o.stats.list(Rng.uniform(0, 4)))
                case NONE => ()
            }
            drop
        }
    }

    def superAction (o: Organism): Unit = {}

    def use (o: Organism, t: Organism) = {
        if (isUsable(o)) {
            action(o, t)
            publish(UsedItem(this, o, cost_type))
            destroy // one-time use only
        }
    }

    def levelUp: Unit = { level += 1 }
    def levelDown: Unit = { level -= 1 }

    // Game evolution: after each turn items move/are used
    def step: Unit = {
        owner match {
            case OrgOwned(orga) => {
                if (isUsable(orga)
                && Rng.choice(level / max_lvl)
                && Rng.choice(orga.stats.decisiveness.residual / 100)) {
                    use(orga, orga)
                }
            }
            case _ => ()
        }
    }
}

// item builder to not store actual items in item drop probability distributions
object MakeItem extends Enumeration {
    type MakeItem = Value
    val KNIFE = Value
    val ALCOHOL = Value
    val MOVE = Value
    val JAVEL = Value
    val HEAT = Value
    val SPIKE = Value
    val LEAK = Value
    val MEMBRANE = Value
    val KEY = Value
    val NONE = Value

    def build_item (it: MakeItem, owner: Owner) {
        it match {
            case KNIFE => new Knife(owner)
            case ALCOHOL => new Alcohol(owner)
            case MOVE => new BodyMovement(owner)
            case JAVEL => new Javel(owner)
            case HEAT => new Heat(owner)
            case SPIKE => new Spike(owner)
            case LEAK => new CytoplasmLeak(owner)
            case MEMBRANE => new MembraneReplacement(owner)
            case KEY => new Key(owner)
            case NONE => return
        }
    }
}
import MakeItem._

// Partition the Items according to their application area:
// Action on local area + straight movement (bounces on walls)
abstract class SpatialActionItem (_owner: Owner) extends Item(_owner) {
    var (mv_vert, mv_horiz): Tuple2[Int, Int] = Rng.weightedChoice(Buffer(
        (0.1, (1,0)), (0.1, (0,1)), (0.1, (-1,0)), (0.1, (0,-1)),
        (0.1, (1,1)), (0.1, (1,-1)), (0.1, (-1,1)), (0.1, (-1,-1)),
        (0.05, (2,1)), (0.05, (-2,1)), (0.05, (-1,2)), (0.05, (-2,-1)),
    )).get
    var moveProba: Double = 0.2
    var durability: Int = 3

    var radius: Int = 0
    def setRadius (r: Int): Unit = { radius = r }

    def LocsPicking: ListBuffer[Pos] = { // positions affected by the item
        var lst = ListBuffer[Pos]()
        val pos: Pos = owner match {
            case PosOwned(position) => position
            case _ => return ListBuffer()
        }
        for (i <- pos.i - radius to pos.i + radius) {
            for (j <- pos.j - radius to pos.j + radius) {
                if (0 <= i && i < pos.room.rows && 0 <= j && j < pos.room.cols) {
                    if (pow(pos.i - i, 2) + pow(pos.j - j, 2) <= radius) {
                        lst += pos.room.locs(i, j)
                    }
                }
            }
        }
        lst
    }

    override def drop: Unit = {
        super.drop
        pickable = false
    }

    override def action(o: Organism, t: Organism): Unit = {
        unpayCost(o)
        super.action(o, t)
    }

    def move: Unit = {
        owner match {
            case PosOwned(pos) => {
                if (!Rng.choice(moveProba)) return
                val newPos = pos.jump(mv_vert, mv_horiz)
                if (newPos != null) {
                    setOwner(PosOwned(newPos))
                    newPos.notification
                    return
                }
                durability -= 1
                if (durability <= 0) {
                    destroy
                } else {
                    val tmp = mv_vert
                    mv_vert = -mv_horiz
                    mv_horiz = tmp
                }
            }
            case _ => ()
        }
    }

    override def step: Unit = { // damage to nearby area
        if (pickable == false) {
            damageUpdate
            for (l <- LocsPicking) {
                l.notification
                for (orga <- l.organisms.toList) {
                    for (o <- orga.toList) {
                        if (OrgOwned(o) != owner) {
                            o.inflictDamage(damage, ItemEffectKill())
                        }
                    }
                }
            }
        }
        super.step
        move
    }
}

// Action on every ( spawner | cell | virus | organism ) of the map, limited nb of uses
abstract class GlobalActionItem (_owner: Owner) extends Item(_owner) {
    override def action (o: Organism, t: Organism): Unit = {
        val pos: Pos = owner match {
            case OrgOwned(orga) => orga.position
            case PosOwned(position) => position
            case _ => { null }
        }
        if (pos != null) {
            for (o <- pos.room.body.organisms) { super.action(o, o) }
        }
        drop
    }
}


/* --- * SpatialActionItem * ---*/
// Weakens organisms it comes into contact with
class Alcohol (_owner: Owner) extends SpatialActionItem(_owner) {
    cost_type = HP
    cost_factor = 10
    damage_factor = 20
    targetStat = HP

    override def toString = "Alcohol"
    def sacrificeValue = 100
}

// Kills organisms it crosses
class Knife (_owner: Owner) extends SpatialActionItem(_owner) {
    setRadius(3)
    pickable = false

    override def step: Unit = {
        for (l <- LocsPicking) {
            l.notification
            for (orga <- l.organisms.toList) {
                for (o <- orga.toList) {
                    o.kill(ItemEffectKill())
                }
            }
        }
        move
    }

    override def toString = "Knife"
    def sacrificeValue = 100
}



/* --- * GlobalActionItem * ---*/

// Randomly moves all organisms around
class BodyMovement (_owner: Owner) extends GlobalActionItem(_owner) {
    cost_type = HP
    cost_factor = 10
    damage_factor = 5
    targetStat = HP

    override def action (o: Organism, t: Organism): Unit = {
        owner match {
            case PosOwned(position) => {
                for (org <- position.room.body.organisms) {
                    unpayCost(o)
                    super.action(o, org)
                    import Direction._
                    for (i <- 1 to 10) {
                        for (dir <- List(UP, DOWN, LEFT, RIGHT)) {
                            org.maybeMove(position.room, dir)
                        }
                    }
                }
            }
            case _ => ()
        }
    }

    override def toString = "Movement"
    def sacrificeValue = 60
}

// Kills on its position
class Javel (_owner: Owner) extends GlobalActionItem(_owner: Owner) {
    override def action (o: Organism, t: Organism): Unit = {
        val pos = position
        if (pos != null) {
            for (org <- pos.organisms(0).toList) org.kill(ItemEffectKill())
            for (org <- pos.organisms(1).toList) org.kill(ItemEffectKill())
        }
        drop
    }

    override def toString = "Javel"
    def sacrificeValue = 100
}

// Slows down cells
class Heat (_owner: Owner) extends GlobalActionItem(_owner) {
    cost_type = HP
    cost_factor = 10
    damage_factor = -5
    targetStat = SPD

    override def toString = "Heat"
    def sacrificeValue = 90
}



/* --- * LocalActionItem * --- */

// Improves cells or viruses depending on who picked it up
class MembraneReplacement (_owner: Owner) extends Item (_owner) {
    cost_type = SPD
    cost_factor = 10
    targetStat = HP
    damage_factor = 20

    override def toString = "Membrane"
    def sacrificeValue = 40
}

// Strengthens viruses
class Spike (_owner: Owner) extends Item (_owner) {
    cost_type = SPD
    cost_factor = 3
    targetStat = POW
    damage_factor = 20

    override def toString = "Spike"
    def sacrificeValue = 70
}

// Cell: spd++, hp--; Virus: unusable ;; newspeed.residual = speed.residual_factor * level ++ base_speed.residual
class CytoplasmLeak (_owner: Owner) extends Item (_owner) {
    cost_type = HP
    cost_factor = 20
    targetStat = SPD
    damage_factor = 20

    override def toString = "Leak"
    def sacrificeValue = 70
}

class Key (_owner: Owner) extends Item(_owner) {
    override def pickUp (o: Organism): Boolean = {
        o.position.room.listenTo(this)
        publish(PickedUpItem(this, o))
        destroy
        false
    }
    override def step {
        super.step
        val pos = position
        if (pos != null) pos.notification
    }

    override def toString = "Key"
    def sacrificeValue = 0
}

class CompactInventory() {
    var contents: HashMap[MakeItem, Int] = new HashMap()
    
    def add(i: Item) {
        val id: MakeItem.MakeItem = i match {
            case _:Alcohol => ALCOHOL
            case _:BodyMovement => MOVE
            case _:Javel => JAVEL
            case _:Heat => HEAT
            case _:Spike => SPIKE
            case _:CytoplasmLeak => LEAK
            case _:MembraneReplacement => MEMBRANE
            case _ => MakeItem.NONE
        }
        if (id != MakeItem.NONE) {
            if (contents.contains(id)) {
                contents(id) += 1
            } else {
                contents(id) = 1
            }
        }
    }

    def compress (inventory: Set[Item]): CompactInventory = {
        inventory.foreach(add(_))
        this
    }

    def decompress(player: Player): Set[Item] = {
        var inventory: Set[Item] = Set()
        val owner = PlayOwned(player)
        for ((it, num) <- contents) {
            for (j <- 1 to num) {
                val instance = it match {
                    case ALCOHOL => new Alcohol(owner)
                    case MOVE => new BodyMovement(owner)
                    case JAVEL => new Javel(owner)
                    case HEAT => new Heat(owner)
                    case SPIKE => new Spike(owner)
                    case LEAK => new CytoplasmLeak(owner)
                    case MEMBRANE => new MembraneReplacement(owner)
                    case _ => null
                }
                if (instance != null) inventory.add(instance)
            }
        }
        inventory
    }
}


// vim: set expandtab tabstop=4 shiftwidth=4 :
