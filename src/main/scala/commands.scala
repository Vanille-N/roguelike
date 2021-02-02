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

    def trySet (args: Array[String]) : Unit = {
        args(0) match {
            case "health" => {
                val pos_x = args(1).toInt
                val pos_y = args(2).toInt
                val target_health = args(3).toInt
                var target_organism: Organism = null

                val tmp_str = room.locs(pos_x, pos_y).listContents

                castle.logs.text += "\n" + tmp_str
                if(tmp_str != "At position (" + pos_y + "," + pos_x + ")\n  empty\n") {
                    val target_int: Int = this.commandRequest("\nWhat cell do you choose ?\n" + prompt).toInt

                    var i = 0
                    if (room.locs(pos_x, pos_y).organisms(1).size > 0) {
                        room.locs(pos_x, pos_y).organisms(1).foreach(o => {
                            if(i == target_int) { target_organism = o }
                            i += 1
                            })
                    }
                    if (room.locs(pos_x, pos_y).organisms(0).size > 0) {
                        room.locs(pos_x, pos_y).organisms(0).foreach(o => {
                            if(i == target_int) { target_organism = o }
                            i += 1
                            })
                    }

                    if(target_organism == null) {
                        castle.logs.text += "\nError : unable to locate target organism :/"
                    } else {
                        target_organism.stats.health = target_health
                    }
                }
            }
            case _ => { castle.logs.text += "\n\tUnknown setting :/" }
        }
    }

    def commandRequest (s: String): String = {
        var ret = ""
        if (status == 0 ) {
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
        } else {
            castle.logs.text += "\n" + "?" + prompt + s
            ret = castle.cmdline.text
        }
        castle.cmdline.text = ""
        ret
    }
}
// vim: set expandtab tabstop=4 shiftwidth=4 :
