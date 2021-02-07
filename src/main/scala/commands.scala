import java.util.concurrent.TimeUnit
import akka.actor._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.FileNotFoundException
import java.io.IOException
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

    // Stores args for the next function to be executed
    var next = new Array[String](10)

    // handle the auto-play
    def scheduler: Scheduler = ActorSystem.create("timer-example").scheduler
    var runner: Cancellable = null


    def tryMove (dir: Direction): Unit = {
        if (player.move(dir)) {
            castle.logs.text += prompt + "Player goes " + dir + "\n"
            castle.logs.text += "-> " + player.position.i + ", " + player.position.j + "\n"
        } else {
            castle.logs.text += "\t> Player cannot go " + dir + "\n"
        }
        room.locs.map(_.updateVisuals)
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
                args.length match {
                case 3 => { // The command is already complete: non-interactive mode
                    val target_id = args(1).toInt
                    val new_value = args(2).toInt
                    val target_organism = getOrganismById(target_id)
                    val stat = args(0) match {
                        case "SPD" => { target_organism.stats.speed }
                        case "HP" => { target_organism.stats.health }
                        case "POW" => { target_organism.stats.power }
                        case "DEF" => { target_organism.stats.resistance }
                        case "DEC" => { target_organism.stats.decisiveness }
                        case _ => { castle.logs.text += "\nError: unbound value " + args(0) + " ;:("; null }
                    }
                    if (stat != null) {
                        stat.base = new_value
                        stat.syncBase
                    }
                    castle.logs.text += "\n" + target_organism
                }
                case 1 => { // The command is incomplete => ask id; ask value (whatever the initial command was).
                    status = 1 // The next call will be passed to the second step
                    next(0) = "set"
                    next(1) = args(0)
                    castle.logs.text += "\n" + prompt + "Which organism do you want to affect? (type l to list the organisms)"
                }
                case _ => { castle.logs.text += "\nInternal error: `set` missing argument" }
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
            val stat = next(1) match {
                    case "SPD" => { target_organism.stats.speed }
                    case "HP" => { target_organism.stats.health }
                    case "POW" => { target_organism.stats.power }
                    case "DEF" => { target_organism.stats.resistance }
                    case "DEC" => { target_organism.stats.decisiveness }
                    case _ => { castle.logs.text += "\nError: unbound value " + args(0) + " ;:("; null }
            }
            if (stat != null) {
                stat.base = new_value
                stat.syncBase
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
            } catch {
                case e: FileNotFoundException => println("Error: Help file not found")
                case e: IOException => println("Error: Failed to open help file")
            }
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

    def step (arg: Array[String]) : Unit = {
        castle.logs.text += "\n"
        if(arg.length == 0) { castle.step }
        else {
            for(i <- 1 to (arg(0).toInt)) { castle.step }
        }
    }

    def play (arg: Array[String]) : Unit = {
        castle.logs.text += "\n"
        if(castle.isPlaying) {return ()}
        castle.isPlaying = true
        if(arg.length == 0) { runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(1,TimeUnit.SECONDS)) { castle.step } }
        else {
            runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(arg(0).toInt,TimeUnit.SECONDS)) { castle.step }
        }
    }

    def stop : Unit = {
        if(castle.isPlaying) {runner.cancel(); castle.isPlaying = false}
        else { () }
    }

    def commandRequest (s: String): Unit = {
        if (status == 0 ) {
            main_command = s
            if (s != "" && castle.cmdline.text != "") castle.logs.text += "\n$ " + s
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
                case "quit" =>  { stop; sys.exit(0) }
                case "Q" =>     { stop; sys.exit(0) }
                case "q" =>     { castle.logs.text += "\n"; castle.globalPanel.requestFocusInWindow() }
                case "clear" => { castle.logs.text = "" }
                // Game interaction
                case "step" =>  { step (s.split(" ").tail) }
                case "N" =>     { castle.step }
                case "play" =>  { play (s.split(" ").tail) }
                case "stop" =>  { stop }
                case "Space" =>{
                    if(castle.isPlaying) {
                        stop
                    } else {
                        play (Array[String]("1"))
                    }
                }
                case "Espace" =>{
                    if(castle.isPlaying) {
                        stop
                    } else {
                        play (Array[String]("1"))
                    }
                }
                // Misc
                case "list" =>  { list }
                case "O" =>     { list }
                case "show" =>  { show (s.split(" ").tail) }
                case "set" =>   { trySet (s.split(" ").tail) }
                // Misc'
                case "help" =>  { help (s.split(" ").tail) }
                case "?" =>     { help (s.split(" ").tail) }
                case "" =>      {}
                case _ =>       { if(castle.cmdline.text != "" ) {castle.logs.text += "\t> command not found ;/\n"} }
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
