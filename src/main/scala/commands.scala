import Direction._
import scala.io.Source

class Command (val castle:Castle, val room: Room, val player: Player) {
    // Status :=
    // | 0 -> waiting for a new command
    // | 1 -> waiting for an answer to a prompt
    // | waiting for a confirmetion on a particular action
    var status: Int = 0
    var main_command: String = null

    var prompt: String = "\t>"

    // Stores the next functions to execute
    var next = new Array[String](10)

    def tryMove (dir: Direction): Unit = {
        if (player.move(dir)) {
            castle.logs.text += prompt + "Player goes " + dir + "\n"
            castle.logs.text += "-> " + player.position.x + ", " + player.position.y + "\n"
        } else {
            castle.logs.text += "\t> Player cannot go " + dir + "\n"
        }
        room.locs.map(_.update)
    }

    def getOrganismById (id: Int): Organism = {
        val lily : List[Organism] = castle.organisms.toList
        if(id > lily.length) { null }
        else {
            var i: Int = 0
            for ( o <- lily ) {
                if (i == id) {return o}
                i += 1
            }
        }
        null
    }

    def list: Unit = {
        var i: Int = 0
        for ( o <- castle.organisms.toList ) {
            castle.logs.text += "\n" + i + "-\t" + o
            i += 1
        }
    }

    def trySet (args: Array[String]) : Unit = {
        status match {
            case 0 => {
                if(args.length == 3) { // The command is already complete: non-interactive mode
                    val target_id = args(1).toInt
                    val new_value = args(2).toInt
                    val target_organism = getOrganismById(target_id)
                    args(0) match {
                        case "speed" =>      { target_organism.stats.speed = new_value }
                        case "health" =>     { target_organism.stats.health = new_value }
                        case "power" =>      { target_organism.stats.power = new_value }
                        case "resistance" => { target_organism.stats.resistance = new_value }
                        case "decisiveness" => { target_organism.stats.decisiveness = new_value }
                        case _ =>            { castle.logs.text += "\nError: unbound value " + args(0) + " ;:(" }
                    }
                    castle.logs.text += "\n" + target_organism
                } else { // The command is incomplete => ask id; ask value (whatever the initial command was).
                    status = 1 // The next call will be passed to the second step
                    next(0) = "set"
                    next(1) = args(0)
                    castle.logs.text += "\n" + prompt + "Which organism do you want to affect? (type l to list the organisms)"
                }
            }
            case 1 => {
                if(args(0) == "l") { list }
                else {
                    status = 2
                    next(2) = args(0)
                    castle.logs.text += "\n" + prompt + "What is the new value of " + next(1) + "?"
                }
            }
            case 2 => {
                val target_id = next(2).toInt
                val new_value = args(0).toInt
                val target_organism = getOrganismById(target_id)
                //castle.logs.text += "\n\n\n" + target_organism
                next(1) match {
                        case "speed" =>      { target_organism.stats.speed = new_value }
                        case "health" =>     { target_organism.stats.health = new_value }
                        case "power" =>      { target_organism.stats.power = new_value }
                        case "resistance" => { target_organism.stats.resistance = new_value }
                        case "decisiveness" => { target_organism.stats.decisiveness = new_value }
                        case _ =>            { castle.logs.text += "\nError: unbound value " + args(0) + " ;:(" }
                }
                castle.logs.text += "\n\n\n" + target_organism
                status = 0
            }
            case _ => {
                castle.logs.text += "\nInternal error: command.trySet entered with status > 2 ;:)"
                status = 0
            }
        }
    }

    def show (arg: Array[String]) : Unit = { if(arg.length == 1) { castle.logs.text += "\n" + getOrganismById(arg(0).toInt) } }

    def help (args: Array[String]): Unit = {
        if(args.length == 0) {
            try {
                castle.logs.text += "\n"
                val src = Source.fromFile("help/help")
                src.foreach { s => castle.logs.text += s }
                src.close
            } finally { castle.logs.text += "Internal Error: help unavailable" }
        } else {
            for (i <- args) {
                try {
                    castle.logs.text += "\n"
                    val src = Source.fromFile("help/help." + i)
                    src.foreach { s => castle.logs.text += s }
                    src.close
                } catch { case e: java.io.FileNotFoundException => castle.logs.text += "Internal Error: help unavailable for `" + i + "`" }
            }
        }
    }

    def commandRequest (s: String): Unit = {
        if (status == 0 ) {
            main_command = s
            if (s != "") castle.logs.text += "\n$ " + s
            s.split(" ")(0) match  {
                // Movements
                case "Up" =>    { tryMove(UP) }
                case "K" =>     { tryMove(UP) }
                case "Down" =>  { tryMove(DOWN) }
                case "J" =>     { tryMove(DOWN) }
                case "Right" => { tryMove(RIGHT) }
                case "L" =>     { tryMove(RIGHT) }
                case "Left" =>  { tryMove(LEFT) }
                case "H" =>     { tryMove(LEFT) }
                // Exiting / functionnalities
                case "quit" =>  { sys.exit(0) }
                case "Q" =>     { sys.exit(0) }
                case "q" =>     { castle.logs.text += "\n"; castle.globalPanel.requestFocusInWindow() }
                case "clear" => { castle.logs.text = "" }
                // Game interaction
                case "step" =>  { castle.step }
                case "N" =>     { castle.step }
                case "list" =>  { list }
                case "L" =>     { list }
                case "show" =>  { show (s.split(" ").tail) }
                case "set" =>   { trySet (s.split(" ").tail) }
                // Misc
                case "help" =>  { help (s.split(" ").tail) }
                case "?" =>     { help (s.split(" ").tail) }
                case "" =>      {}
                case _ =>       { castle.logs.text += "\t> command not found ;/\n" }
            }
        } else {
            castle.logs.text += "\n" + "?" + prompt + next(0) + ".ans\t<-\t" + s
            next(0) match {
                case "set" => { trySet (castle.cmdline.text.split(" ")) }
                case _ => { castle.logs.text += "\nInternal error: command.trySet entered with status > 2 ;:)" }
            }
        }
        castle.cmdline.text = ""
    }
}
// vim: set expandtab tabstop=4 shiftwidth=4 :
