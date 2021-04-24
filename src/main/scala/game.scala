import scala.collection.mutable.Set
import scala.swing._
import event._
import ArtefactType._

class BodyPart (val level: Level, val players: List[Player])
extends Reactor with Publisher {
    var organisms: Set[Organism] = Set()
    var items: Set[Item] = Set()
    var nb_destroyed_items: Int = 1  // 1 avoid a division by 0
    var organismsBarycenter: Array[Pos] = Array(null, null)

    val room = level.makeRoom(this, players)

    var repeat: Int = 1
    var isPlaying: Boolean = false
    
    // main loop
    def step {
        organisms.foreach(o => o.stats.syncCurrent)
        val () = { // update barycenter
            var count: Array[Int] = Array(0, 0)
            val i = Array(0, 0)
            val j = Array(0, 0)
            for (o <- organisms) {
                val idx = if (o.isFriendly) 1 else 0
                i(idx) += o.position.i
                j(idx) += o.position.j
                count(idx) += 1
            }
            for (idx <- 0 to 1) {
                organismsBarycenter(idx) = room.locs(i(idx) / count(idx).max(1), j(idx) / count(idx).max(1))
            }
        }
        var active = true
        // loop until no one can move anymore
        while (active) {
            active = false
            organisms.foreach(o => active = o.step(room) || active)
            room.locs.map(_.battle)
        }
        // items progress
        room.locs.map(_.artefacts.foreach(_.step))
        items.foreach(_.step)
        // viruses age
        // + synchronize effects of items and damage
        // + remove all organisms that should die
        organisms.foreach(o => {
            if (o.isFriendly && Rng.choice(0.07)) {
                o.inflictDamage(1, CauseOfDeath.OldAge)
            }
            o.sync
        })
        room.locs.map(_.trySpawn(organisms.size)) // sometimes spawn new organisms

        publish(LoopStep())
    }

    listenTo(room)

    reactions += {
        case PickedUpItem(i, o) => { if (i.isInstanceOf[Key]) publish(PickedUpKey(o)) }
        case DyingItem (i: Item) => {
            deafTo(i)
            nb_destroyed_items = nb_destroyed_items + 1;
            def get_artefact_type: ArtefactType = {
                return Rng.uniform(0, 6) match {
                    case 0 => LEVELUP
                    case 1 => LEVELDOWN
                    case 2 => LEVELSET
                    case 3 => LEVELDOUBLE
                    case 4 => LEVELDDOUBLE
                    case 5 => NONE
                }
            }
            if(Rng.choice(items.size / nb_destroyed_items)
                    && i.position != null) {
                Rng.uniform(0, 5) match {
                    case 0 => i.position.artefacts =
                        i.position.artefacts.+(
                            new Artefact(i.position,
                                Rng.uniform(0, 5),
                                Rng.uniform(0, 5),
                                get_artefact_type))// Artefact
                    case 1 => i.position.artefacts =
                        i.position.artefacts.+(
                            new Murderer(i.position,
                                Rng.uniform(0, 5),
                                Rng.uniform(0, 5),
                                get_artefact_type))// Murderer
                    case 2 => i.position.artefacts =
                        i.position.artefacts.+(
                            new ForceUsage(i.position,
                                Rng.uniform(0, 5),
                                Rng.uniform(0, 5),
                                get_artefact_type))// Force Usage
                    case 3 => i.position.artefacts =
                        i.position.artefacts.+(
                            new Temptation(i.position,
                                Rng.uniform(0, 5),
                                Rng.uniform(0, 5),
                                get_artefact_type))// Temptation
                    case 4 => i.position.artefacts =
                        i.position.artefacts.+(
                            new Unattach(i.position,
                                Rng.uniform(0, 5),
                                Rng.uniform(0, 5),
                                get_artefact_type))// Unattach
                }
            }
        }
    }
}


