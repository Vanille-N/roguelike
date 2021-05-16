import swing._

abstract class WinCondition (val player: Player)
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


import scala.reflect.ClassTag
class WinByPickup(
    val body: BodyPart, _player: Player,
    val pickupCount: Int,
) extends WinCondition(_player) {
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
    val body: BodyPart, _player: Player,
    val name: String = "",
    val killCount: Int,
) extends WinCondition(_player) {
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
    val body: BodyPart, _player: Player,
    var path: List[Tuple2[Int, Int]],
    val strengthThreshold: Int,
    val turnCount: Int,
) extends WinCondition(_player) {
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
            if (pos.strength(player.id) > strengthThreshold) {
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
class WinByPosition(
    _body: BodyPart, _player: Player,
    i: Int, j: Int,
    _strengthThreshold: Int,
    _turnCount: Int,
) extends WinByPath(
    _body, _player,
    path=List((i, j)),
    _strengthThreshold,
    _turnCount,
) {
    override def explanation = s"To complete this level, conquer the marked tile\n(stay for $turnCount turns on it with a strength >$strengthThreshold)"
}

class WinLock (
    _player: Player,
    nbPlayers: Int,
    maxScore: Int,
) extends WinCondition(_player) {
    def explanation = ""
    def completion = 100
    override def win {}
    override def message = {
        if (nbPlayers == 1) {
            "========================\n" +
            "#   CONGRATULATIONS    #\n" +
            "#    Game Cleared      #\n" +
            "========================\n"
        } else if (player.score == maxScore) {
            "========================\n" +
            "#   CONGRATULATIONS    #\n" +
            "# You are first place  #\n" +
            "========================\n"
        } else {
            "========================\n" +
           s"Your score: ${player.score}\n" +
           s"High score: $maxScore\n" +
            " You'll do better next time\n"
        }
    }
}

