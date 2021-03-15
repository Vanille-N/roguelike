import scala.swing._

abstract class WinCondition(val body: BodyPart)
extends Reactor with Publisher {
    def win {
        body.logs.text = ""
        body.logs.text += "\n===================\n"
        Thread.sleep(1000)
        publish(new levelClear())
    }
    def explanation: String
    def completion: Int
}

class WinByPosition(body: BodyPart)
extends WinCondition(body) {
    val pos = body.room.locs(25, 25)
    val strengthThreshold = 100
    val turnCount = 50
    def explanation = s"To complete this level, conquer the marked tile\n(stay for $turnCount turns on it with a strength >$strengthThreshold)"
    var count = 0
    
    def completion: Int = {
        count * 100 / turnCount
    }

    listenTo(body)
    reactions += {
        case LoopStep() => {
            pos.notification
            if (pos.strength(1) > strengthThreshold) {
                count += 1
                if (count == turnCount) win
            } else count = 0
        }
    }
}

class WinByPickup(body: BodyPart)
extends WinCondition(body) {
    val pickupCount = 10
    def explanation = s"Pick up $pickupCount hidden items"
    var count = 0
    
    def completion: Int = {
        count * 100 / pickupCount
    }
    listenTo(body)
    reactions += {
        case PickedUpKey(o) => {
            if (o.isFriendly) {
                count += 1
                if (count == pickupCount) win
            }
        }
    }
}

class WinByKillCount(body: BodyPart)
extends WinCondition(body) {
    val killCount = 100
    def explanation = s"Kill $killCount hostile organisms"
    var count = 0

    def completion: Int = {
        count * 100 / killCount
    }
    listenTo(body.room)
    reactions += {
        case OrganismDeath(o, _) => {
            if (!o.isFriendly) {
                count += 1
                if (count == killCount) win
            }
        }
    }
}

