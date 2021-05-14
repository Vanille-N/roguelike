import Math._
import collection.mutable.Set

import swing._
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

class Artefact (val position: Pos, val radius: Int, val level: Int, val artefact_type: ArtefactType)
extends Publisher {
    var remaining_steps: Int = 1.max(level) * 5
    // radius defines the radius of the ciorcle it looks for to find items

    // levelset variable defines the evel of the artefact
    
    def influenceZone: Set[Pos] = {
        var ans: Set[Pos] = Set()
        for (i <- -radius to radius; j <- -radius to radius) {
            if (i * i + j * j < radius * radius) {
                val new_i = position.i + i
                val new_j = position.j + j
                if (0 <= new_i && new_i < position.room.rows && 0 <= new_j && new_j < position.room.cols) {
                    ans += position.room.locs(new_i, new_j)
                }
            }
        }
        ans
    }

    // findOrganism return a set containing the organisms in the reach of the artefact.
    def findOrganism: Set[Organism] = {
        var ans: Set[Organism] = Set()
        influenceZone.foreach(p => {
            ans ++= p.organisms(0)
            ans ++= p.organisms(1)
        })
        ans.filter(_ != null)
    }

    // findItems return a set containing the items in the reach of the artefact.
    def findItems: Set[Item] = {
        var ans: Set[Item] = Set[Item]()
        influenceZone.foreach(p => {
            ans ++= p.items
            for (o <- p.organisms(0)) ans ++= o.items
            for (o <- p.organisms(1)) ans ++= o.items
        })
        ans.filter(_ != null)
    }

    // action dedfines what an artefact should do on a given item.
    def action (i: Item): Unit = {
        artefact_type match {
            case LEVELUP      => { i.level = 6.min(i.level + 1) }
            case LEVELDOWN    => { i.level = 1.max(i.level - 1) }
            case LEVELSET     => { i.level = level }
            case LEVELDOUBLE  => { i.level = 6.min(i.level * 2) }
            case LEVELDDOUBLE => { i.level = 1.max(i.level / 2) }
            case NONE => {}
        }
    }

    def step: Unit = {
        influenceZone.foreach(_.notification)
        for (i <- findItems) action(i)
        remaining_steps = remaining_steps - 1
        if (remaining_steps == 0) 
            position.artefacts = position.artefacts.filter(_ != this)
        publish(StepForward())
    }
    override def toString = "Classical artefact"
}

class Murderer (// Artefact that kills any organisms in its reach
    position: Pos,
    radius: Int,
    level: Int,
    artefact_type: ArtefactType
) extends Artefact(position, radius, level, artefact_type) {
    override def step: Unit = {
        for (o <- findOrganism) {
            if (o.skills.immunity.get < 5 ) {
                o.kill(CauseOfDeath.Artefact)
                o.sync
            }
        }
        super.step
    }

    override def toString = "Murderer artefact"
}

class ForceUsage (// Artefact that make the organisms use their items (even if it harms / kills them)
    position: Pos,
    radius: Int,
    level: Int,
    artefact_type: ArtefactType
) extends Artefact(position, radius, level, artefact_type) {
    override def step: Unit = {
        for (o <- findOrganism) {
            for (i <- o.items) {
                i.action(o, o)
                return ()
            }
        }
        super.step
    }

    override def toString = "ForceUsage artefact"
}

class Temptation (// Artefact that make the organisms check whether use or not their items
    position: Pos,
    radius: Int,
    level: Int,
    artefact_type: ArtefactType
) extends Artefact(position, radius, level, artefact_type) {
    override def step: Unit = {
        for (o <- findOrganism) {
            for (i <- o.items) {
                i.use(o, o)
                return ()
            }
        }
        super.step
    }

    override def toString = "Temptation artefact"
}

class Unattach (// Artefact that make the organisms drop their items
    position: Pos,
    radius: Int,
    level: Int,
    artefact_type: ArtefactType
) extends Artefact(position, radius, level, artefact_type) {
    override def step: Unit = {
        for (o <- findOrganism) {
            for (i <- o.items) i.drop
        }
        super.step
    }

    override def toString = "Unattach artefact"
}

