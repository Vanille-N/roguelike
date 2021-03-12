import scala.swing._

class WinCondition(val body: BodyPart) extends Reactor with Publisher {
    def win {
        publish(new levelClear())
    }
}

class WinByPosition(body: BodyPart)
extends WinCondition(body) {}

class WinByPickup(body: BodyPart)
extends WinCondition(body) {}

class WinByKillCount(body: BodyPart)
extends WinCondition(body) {}

