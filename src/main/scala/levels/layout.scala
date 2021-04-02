class Level (val num: Int, val max: Int) {
    val layoutName = num match {
        case 0 => "plain"
        case 2 => "snake"
        case 3 => "cross"
        case 1 => "boxes"
        case 5 => "brain"
        case 6 => "ending"
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
            case "boxes" => new WinByPath(
                body,
                path=List((2, 2), (25, 2), (15, 15), (2, 15), (25, 25), (2, 25)),
                strengthThreshold=200,
                turnCount=20,
            )
            case _ => new WinLock
        }
    }
}
