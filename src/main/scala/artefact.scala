import scala.collection.mutable.Set
import Math._
import scala.swing._
import event._

case class StepForward() extends Event

object ArtefactType extends Enumeration {
    type ArtefactType = Value
    val LEVELUP = Value ("levelup")
    val LEVELDOWN = Value ("leveldown")
    val LEVELSET = Value ("levelset")
    val LEVELDOUBLE = Value ("level *= 2")
    val LEVELDDOUBLE = Value ("level /= 2")
    val NONE = Value("no action")
}
import ArtefactType._

class Artefact (val position: Pos, val radius: Int, val level: Int, val artefact_type: ArtefactType) extends Publisher {
    // radius defines the radius of the ciorcle it looks for to find items

    // levelset variable defines the evel of the artefact

    def findOrganism: Set[Organism] = {
        var ans: Set[Organism] = Set()
        for (i <- 0 to radius) {
            for (j <- 0 to radius) {
                if((i*i+j*j) < radius*radius) {
                    for (k <- 0 to 1) {
                        // Four locations to check...
                        //ans ++= position.room.locs(position.i - i, position.j - j).organisms(_) does NOT work
                        ans ++= position.room.locs(position.i - i, position.j - j).organisms(k)
                        ans ++= position.room.locs(position.i - i, position.j + j).organisms(k)
                        ans ++= position.room.locs(position.i + i, position.j + j).organisms(k)
                        ans ++= position.room.locs(position.i + i, position.j - j).organisms(k)
                    }
                }
            }
        }
        ans
    }

    def findItems: Set[Item] = {
        var answer: Set[Item] = Set[Item]()
        for (i <- 0 to radius) {
            for (j <- 0 to radius) {
                if(i*i+j*j < radius*radius) {
                    for(k <- 0 to 1) {
                        // Four locations to check...
                        answer ++= position.room.locs(position.i - i, position.j - j).items
                        for (o <- position.room.locs(position.i - i, position.j - j).organisms(k)) answer ++= o.items
                        answer ++= position.room.locs(position.i - i, position.j + j).items
                        for (o <- position.room.locs(position.i - i, position.j + j).organisms(k)) answer ++= o.items
                        answer ++= position.room.locs(position.i + i, position.j + j).items
                        for (o <- position.room.locs(position.i + i, position.j + j).organisms(k)) answer ++= o.items
                        answer ++= position.room.locs(position.i + i, position.j - j).items
                        for (o <- position.room.locs(position.i + i, position.j - j).organisms(k)) answer ++= o.items
                    }
                }
            }
        }
        answer
    }

    // action dedfines what an artefact should do on a given item.
    def action (i: Item): Unit = {
        artefact_type match {
            case LEVELUP      => { i.level += 1 }
            case LEVELDOWN    => { i.level -= 1 }
            case LEVELSET     => { i.level = level }
            case LEVELDOUBLE  => { i.level *= 2 }
            case LEVELDDOUBLE => { i.level /= 2 }
            case NONE => {}
        }
    }

    def step: Unit = {
        for (i <- findItems) action(i)
        publish(StepForward())
    }
}

class Murderer (
    position: Pos,
    radius: Int,
    level: Int,
    artefact_type: ArtefactType
    ) extends Artefact (position, radius, level, artefact_type) {
        //
    override def step: Unit = {
        for (o <- findOrganism) {
            o.stats.health.residual = 0
        }
        super.step
    }
}

class ForceUsage (
    position: Pos,
    radius: Int,
    level: Int,
    artefact_type: ArtefactType
    ) extends Artefact (position, radius, level, artefact_type) {
        //
    override def step: Unit = {
        for (o <- findOrganism) {
            for (i <- o.items) {
                i.action(o, o)
                // Does NOT destroy the item after usage
            }
        }
        super.step
    }
}

class Temptation (
    position: Pos,
    radius: Int,
    level: Int,
    artefact_type: ArtefactType
    ) extends Artefact (position, radius, level, artefact_type) {
        //
    override def step: Unit = {
        for (o <- findOrganism) {
            for (i <- o.items) {
                i.use(o, o)
            }
        }
        super.step
    }
}

class Unattach (
    position: Pos,
    radius: Int,
    level: Int,
    artefact_type: ArtefactType
    ) extends Artefact (position, radius, level, artefact_type) {
        //
    override def step: Unit = {
        for (o <- findOrganism) {
            for (i <- o.items) i.drop
        }
        super.step
    }
}

