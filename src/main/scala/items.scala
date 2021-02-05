object TargetType extends Enumeration {
    type TargetType = Value
    val CELL          = Value("cell")
    val VIRUS         = Value("virus")
    val ORGANISM      = Value("organism")
    val SPAWNER       = Value("spawner")
    val ORG_N_SPAWNER = Value("spawner")
    val ANY           = Value("any")

    def enemy (typ: TargetType): TargetType  = {
        type match {
            case CELL  => VIRUS
            case VIRUS => CELL
            case ANY   => ANY
        }
    }
}

abstract class Item {
    // Global vars
    var level: Int
    var name: String
    var moves: Boolean

    var target_type: TargetType

    var owner: Organism
    var proViruses: Boolean

    def pickUp (o: Organism): unit = {
        if(proViruses != null) {
            if((proViruses && o.isFriendly) || (!proViruses && !o.isFriendly)) {
                o.stats.health /= 2
            }
        }
        owner = o
        if(o.isFriendly) {
            proViruses = false
        } else {
            proViruses = true
        }
    }
    def drop: Unit = {
        if (owner != null) {
            owner = null
        }
        setPos(owner.position)
    }

    def setMovable: Unit = { moves = true }
    def setImmovable: Unit = { moves = false }

    // Defines the cost of the (default) use of the itm
    // + the Stats fiels the user will cast
    val cost: Int
    val src_payment: String

    def isUsable (o: Organism): Boolean

    // defines the action.
    // o is the target of an item (owner or not)
    def action(o: Organism) : Unit

    // Space-related variables
    var position: Pos
    var action_radius: Int

    // Game evolution
    def step: Unit = {
        if(owner != null) {
            val r = new Random
            if(isUsable && r.nextInt(100) > owner.stats.decisiveness) {
                action(owner) // lorsque l'item prend la décision de se faire utiliser, c'est toujours sur l'owner
            } else {
                if(isUsable && r.nextInt(100) > owner.stats.decisiveness) {
                    drop
                }
            }
        }
    }

    // Position of the item on the map (null if owned / worn)
    def setPos (new_p: Pos) = { p = new_p }
}



// Partition the Items according to their application area:
abstract class SpatialActionItem extends Item {// Action on local area + straight movement
    val mvt_param_a: Int
    val mvt_param_b: Int

    def LocsPicking: Unit

    def setActionRadius (r: Int): Unit = { action_radius = r }
}

abstract class GlobalActionItem extends Item {// Action on every ( spawner | cell | virus | organism ) of the map, limited nb of uses
    action_radius = -1
}

abstract class LocalActionItem extends Item {// Action on a particular org
    action_radius = 0
}



/* --- * SpatialActionItem * ---*/
class Alcohol (val pos_ini: Pos, val damages: Int, val speed: int) extends SpatialActionItem { // Affaiblie tout sur son passage
    /**/
}

class Knife (val pos_ini: Pos, val damages: Int, val speed: int, val width:Int, val height:Int) extends SpatialActionItem { // Tue tous les organismes et altère les spawners
    /**/
}



/* --- * GlobalActionItem * ---*/
class BodyMovement (val pos_ini: Pos, val damages_factor: Int) extends GlobalActionItem { // Déplace tous les organismes aléatoirement et détériore les spawners
    /**/
}

class Javel (val pos_ini: Pos, val speed: int) extends GlobalActionItem { // Tue tous les organisms, et détériore les spawners  dès que ramassée
    /**/
}

class Heat (val pos_ini: Pos, val temp: Int) extends GlobalActionItem { // Améliore les spawners + ralentit les cellules;
    /**/
}



/* --- * LocalActionItem * --- */
class MembraneReplacement (val pos_ini: Pos, val health_factor: Int) extends LocalActionItem {// Améliore les cellules, renforce les virus (ils peuvent se faire passer pour des gentils maintenant => immunité) ;; newhealth = health_factor * level + health
}

class Spike (val pos_ini: Pos, val damages: Int) extends LocalActionItem {// Renforce les virus
}

class CytoplasmLeak (val pos_ini: Pos, val speed_factor: Int) extends LocalActionItem {// Cell: spd++, hp--; virus: non utilisable ;; newspeed = speed_factor * level ++ base_speed
}

// vim: set expandtab tabstop=4 shiftwidth=4 :
