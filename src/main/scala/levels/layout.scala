class Level (val num: Int) {
    def makeRoom (body: BodyPart): Room = {
        val layoutName = num match {
            case 1 => "plain"
            case 2 => "cross"
            case 3 => "snake"
        }
        new Room(body, layoutName)
    }
    def makeWinCondition (body: BodyPart): WinCondition = {
        num match {
            case 1 => new WinByPosition(body)
            case 2 => new WinByPickup(body)
            case 3 => new WinByKillCount(body)
        }
    }
}
