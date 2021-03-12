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
}

class WinByPosition(body: BodyPart)
extends WinCondition(body) {
    val pos = body.room.locs(25, 25)
    def explanation = "To complete this level, conquer the marked tile\n(stay for 50 turns on it with a strength >100)"
    var count = 0
    listenTo(body)
    reactions += {
        case LoopStep() => {
            pos.notification
            if (pos.strength(1) > 100) {
                count += 1
                if (count == 50) win
            } else count = 0
        }
    }
}

class WinByPickup(body: BodyPart)
extends WinCondition(body) {
    def explanation = "Pick up three hidden items"
}

class WinByKillCount(body: BodyPart)
extends WinCondition(body) {
    def explanation = "Kill 500 hostile organisms"
}

