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
extends WinCondition(body) {}

class WinByPickup(body: BodyPart)
extends WinCondition(body) {}

class WinByKillCount(body: BodyPart)
extends WinCondition(body) {}

