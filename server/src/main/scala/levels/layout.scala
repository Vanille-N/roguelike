class Level (val num: Int, val max: Int) {
    val layoutName = num match {
        case 1 => "plain"
        case 2 => "snake"
        case 3 => "cross"
        case 4 => "boxes"
        case 5 => "brain"
        case 6 => "ending"
    }
    def makeRoom (body: BodyPart, players: List[Player]): Room = {
        new Room(body, layoutName, players)
    }
    def makeWinCondition (body: BodyPart, player: Player): WinCondition = {
        layoutName match {
            case "brain" => new WinByKillCount(
                body, player, "neuron",
                killCount=5,
            )
            case "plain" => new WinByPosition(
                body, player, i=25, j=25,
                _strengthThreshold=100,
                _turnCount=50,
            )
            case "cross" => new WinByPickup(
                body, player,
                pickupCount=10,
            )
            case "snake" => new WinByKillCount(
                body, player,
                killCount=100,
            )
            case "boxes" => new WinByPath(
                body, player,
                path=List((2, 2), (25, 2), (15, 16), (2, 15), (25, 25), (2, 25)),
                strengthThreshold=200,
                turnCount=20,
            )
            case _ => new WinLock(player)
        }
    }
}
