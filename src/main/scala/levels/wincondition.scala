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
    def explanation = "Pick up three hidden items"
    def completion: Int = 0
}

class WinByKillCount(body: BodyPart)
extends WinCondition(body) {
    def explanation = "Kill 500 hostile organisms"
    def completion: Int = 0
}

