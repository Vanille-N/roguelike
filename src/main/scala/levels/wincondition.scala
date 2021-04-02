import scala.swing._

abstract class WinCondition(val body: BodyPart)
extends Reactor with Publisher {
    def win {
        publish(ClearLogs())
        Thread.sleep(1000)
        publish(new LevelClear())
    }
    def explanation: String
    def completion: Int

    def message: String = {
        "==============================\n" +
        "             GOAL             \n" +
                     explanation           +
      "\n==============================\n"
    }
}

class WinByPosition(
    body: BodyPart, i: Int, j: Int,
    strengthThreshold: Int,
    turnCount: Int,
) extends WinCondition(body) {
    val pos = body.room.locs(i, j)
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

import scala.reflect.ClassTag
class WinByPickup(
    body: BodyPart,
    pickupCount: Int,
) extends WinCondition(body) {
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

class WinByKillCount(
    body: BodyPart,
    name: String = "",
    killCount: Int,
) extends WinCondition(body) {
    def explanation = s"Kill $killCount ${if (name == "") "hostile" else name} organisms"
    var count = 0

    def completion: Int = {
        count * 100 / killCount
    }
    listenTo(body.room)
    reactions += {
        case OrganismDeath(o, _) => {
            if (!o.isFriendly && (name == "" || o.name == name)) {
                count += 1
                if (count == killCount) win
            }
        }
    }
}

class WinByPath(
    body: BodyPart,
    var path: List[Tuple2[Int, Int]],
    strengthThreshold: Int,
    turnCount: Int,
) extends WinCondition(body) {
    def explanation = s"Conquer all successive positions"
    val maxProgress = path.length * turnCount
    var count = 0
    var num = 0
    def completion: Int = {
        (num * turnCount + count) * 100 / maxProgress
    }
    
    listenTo(body)
    reactions += {
        case LoopStep() => {
            val pos = body.room.locs(path(num)._1, path(num)._2)
            pos.notification
            if (pos.strength(1) > strengthThreshold) {
                count += 1
                if (count == turnCount) {
                    if (num == path.length - 1) {
                        win
                    } else {
                        num += 1
                    }
                }
            } else count = 0
        }
    }
}

class WinLock ()
extends WinCondition(null) {
    def explanation = ""
    def completion = 100
    override def win {}
    override def message = {
        "========================\n" +
        "#   CONGRATULATIONS    #\n" +
        "#    Game Cleared      #\n" +
        "========================\n"
    }
}

