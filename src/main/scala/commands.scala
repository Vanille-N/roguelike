import Direction._

class Command (val castle:Castle, val room: Room, val player: Player) {
    // Status :=
    // | 0 -> waiting for a new command
    // | 1 -> waiting for an answer to a prompt
    // | waiting for a confirmetion on a particular action
    var status: Int = 0

    var prompt: String = "\t>"

    // Stores the next functions to execute
    var next = Nil

    def tryMove (dir: Direction): Unit = {
        if (player.move(dir)) {
            castle.logs.text += prompt + "Player goes " + dir + "\n"
            castle.logs.text += "-> " + player.position.x + ", " + player.position.y + "\n"
        } else {
            castle.logs.text += "\t> Player cannot go " + dir + "\n"
        }
        room.locs.map(_.update)
    }

    def commandRequest (s: String) = {
        if (s != "") castle.logs.text += "$ " + s
        s.split(" ")(0) match  {
            case "Up" =>    { tryMove(UP) }
            case "Down" =>  { tryMove(DOWN) }
            case "Right" => { tryMove(RIGHT) }
            case "Left" =>  { tryMove(LEFT) }
            case "quit" =>  { sys.exit(0) }
            case "q" =>     { castle.globalPanel.requestFocusInWindow() }
            case "clear" => { castle.logs.text = "" }
            case "set" =>   {}
            case "" =>      {}
            case _ =>       { castle.logs.text += "\t> command not found ;/\n" }
        }
        castle.cmdline.text = ""
    }
}
// vim: set expandtab tabstop=4 shiftwidth=4 :
