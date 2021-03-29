class Level (val num: Int, val max: Int) {
    val layoutName = num match {
        case 1 => "plain"
        case 2 => "cross"
        case 3 => "snake"
        case 4 => "brain"
        case 5 => "ending"
    }
    def makeRoom (body: BodyPart, startingStats: StatSetGen): Room = {
        new Room(body, layoutName, startingStats)
    }
    def makeWinCondition (body: BodyPart): WinCondition = {
        layoutName match {
            case "brain" => new WinByKillCount(
                body, "neuron",
                killCount=5,
            )
            case "plain" => new WinByPosition(
                body, i=25, j=25,
                strengthThreshold=100,
                turnCount=50,
            )
            case "cross" => new WinByPickup(
                body,
                pickupCount=10,
            )
            case "snake" => new WinByKillCount(
                body,
                killCount=100,
            )
            case _ => new WinLock
        }
    }
}
