import swing._

abstract class WinCondition (player: Player)
extends Reactor with Publisher {
    def win {
        publish(ClearLogs())
        Thread.sleep(1000)
        publish(new LevelClear(player))
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
    body: BodyPart, player: Player,
    i: Int, j: Int,
    strengthThreshold: Int,
    turnCount: Int,
) extends WinCondition(player) {
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
            if (pos.strength(player.id) > strengthThreshold) {
                count += 1
                if (count == turnCount) win
            } else count = 0
        }
    }
}

import scala.reflect.ClassTag
class WinByPickup(
    body: BodyPart, player: Player,
    pickupCount: Int,
) extends WinCondition(player) {
    def explanation = s"Pick up $pickupCount hidden items"
    var count = 0
    
    def completion: Int = {
        count * 100 / pickupCount
    }
    listenTo(body)
    reactions += {
        case PickedUpKey(o) => {
            if (o.asIndex == player.id) {
                count += 1
                if (count == pickupCount) win
            }
        }
    }
}

class WinByKillCount(
    body: BodyPart, player: Player,
    name: String = "",
    killCount: Int,
) extends WinCondition(player) {
    def explanation = s"Kill $killCount ${if (name == "") "hostile" else name} organisms"
    var count = 0

    def completion: Int = {
        count * 100 / killCount
    }
    listenTo(body.room)
    reactions += {
        case OrganismDeath(o, _, VirusKill(i)) => {
            if (i == player.id && (name == "" || o.name == name)) {
                count += 1
                if (count == killCount) win
            }
        }
    }
}

class WinByPath(
    body: BodyPart, player: Player,
    var path: List[Tuple2[Int, Int]],
    strengthThreshold: Int,
    turnCount: Int,
) extends WinCondition(player) {
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

class WinLock (player: Player)
extends WinCondition(player) {
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

