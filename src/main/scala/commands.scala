import java.util.concurrent.TimeUnit
import akka.actor._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.FileNotFoundException
import java.io.IOException
import scala.io.Source
import scala.swing._
import java.awt.Font
import java.lang.System
import event._

import Direction._

// The Status enumeration describes the way a command is called:
//    | FIRST_CALL -> the first time a function is called
//    | SEC_CALL   ->     second
//    | TER_CALL   ->     third
object Status extends Enumeration {
    type Status = Value
    val FIRST_CALL = Value("first call")
    val SEC_CALL = Value("second call")
    val TER_CALL = Value("lthird call")
}
import Status._

object CommandType extends Enumeration {
    type CommandType = Value
    val DIRECTION         = Value("ExecuteDirection")
    val DIGIT             = Value("ExecuteDigit")
    val GAME_INTERACTION  = Value("ExecuteGameInteraction")
    val GAME_MANIPULATION = Value("ExecuteGameManipulation")
    val OTHER             = Value("ExecuteOther")
}
import CommandType._

class Command (val body: BodyPart, val room: Room, val player: Player) {
    // When a key is pressed, it is checked to define the action to perform
    // actionFromText returns the action to perform given a text command.
    def actionFromText  (str: String) : (() => Unit) = {
        if(str == "focus" ) (() => body.cmdline.requestFocusInWindow())
        else (() => commandRequest(str))
    }

    // actionFromKey returns the action to perform given a pressed key
    def actionFromKey (key: Key.Value): ( () => Unit ) = {
        try shortcuts(key)
        catch {
            case e:java.util.NoSuchElementException => {
                //body.logs.text += "\nErreur: touche " + key.toString + " non programmée."
                () => ()
            }
        }
    }

    def keyPressed (c: Key.Value ): Unit = { actionFromKey(c)() }

    def ExecuteDirection (command: String): (() =>Unit) = { () => {
        command match {
            case "Up" => { tryMove(UP) }
            case "Down" => { tryMove(DOWN) }
            case "Left" => { tryMove(LEFT) }
            case "Right" => { tryMove(RIGHT) }
        }
    }}

    def ExecuteDigit (command: String): (() =>Unit) = { () => { repeat = repeat * 10 + command.toInt }}

    def ExecuteGameInteraction (command: String): (() =>Unit) = { () => {
        command match {
                case "step" =>  { step (main_command.split(" ").tail) }
                case "step_multiple" =>     { repeatAction({() => body.step}) }
                case "play" =>  { play (main_command.split(" ").tail) }
                case "stop" =>  { stop }
                case "Space" => {
                    if(body.isPlaying) {
                        stop
                    } else {
                        play (Array[String]("1"))
                    }
                }
        }
    }}

    def ExecuteGameManipulation(command: String): (() =>Unit) = { () => {
        command match {
            // Organisms
            case "list" =>  { list }
            case "show" =>  { show (main_command.split(" ").tail) }
            case "set" =>   { trySet (main_command.split(" ").tail) }
            // Items
            case "itm" =>     { itm (main_command.split(" ").tail) }
            case "itmadd" =>  { itmadd (main_command.split(" ").tail) }
            case "itmdel" =>  { itmdel (main_command.split(" ").tail) }
            case "itmlvl" =>  { itmlvl (main_command.split(" ").tail) }
        }
    }}

    def ExecuteOther(command: String): (() =>Unit) = { () => {
        command match {
            case "Escape" => { repeat = 1 }
            case "quit" =>  { stop; Runtime.getRuntime().halt(0) }
            case "q" =>     { body.logs.text += "\n"; body.globalPanel.requestFocusInWindow() }
            case "clear" => { body.logs.text = "" }
            // Misc'
            case "help" =>  { help (main_command.split(" ").tail) }
            case "?" =>     { help (main_command.split(" ").tail) }
            case "" =>      {}
            case _ =>       { if(body.cmdline.text != "" ) { body.logs.text += "\t> command not found ;/\n" }
            /*else { body.logs.text += "\n"+s }*/ }
        }
    }}
    def commandIsDirection (command: String) : Boolean = {
        val list_of_dirs: List[String] = List("Up", "Down", "Left", "Right")
        if (list_of_dirs.exists ( x => x == command )) true
        else false
    }

    def commandIsDigit (command: String) : Boolean = {
        val list_of_digits: List[String] = List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
        if (list_of_digits.exists ( x => x == command )) true
        else false
    }

    def commandIsGameInteraction (command: String) : Boolean = {
        val list_of_interactions: List[String] = List("play", "step", "stop", "step_multiple", "Space")
        if (list_of_interactions.exists ( x => x == command )) true
        else false
    }

    def commandIsGameManipulation (command: String) : Boolean = {
        val list_of_manipulation: List[String] = List("list", "show", "set", "itm", "itmadd", "itmdel", "itmlvl")
        if (list_of_manipulation.exists ( x => x == command )) true
        else false
    }

    def commandIsOther (command: String) : Boolean = {
        (!commandIsDirection(command)
            && !commandIsDigit(command)
            && !commandIsGameInteraction(command)
            && !commandIsGameManipulation(command)
        )
    }

    def getCommandType (command: String) : CommandType = {
        val filters_list: List[String => Boolean] = List(commandIsDirection, commandIsDigit, commandIsGameInteraction, commandIsGameManipulation, commandIsOther)
        filters_list.indexOf(filters_list.filter( x => x(command) )(0)) match {
            case 0 => DIRECTION
            case 1 => DIGIT
            case 2 => GAME_INTERACTION
            case 3 => GAME_MANIPULATION
            case _ => OTHER
        }
    }

    def boundTypeFun (command_type: CommandType) : (String => (() => Unit)) = {
        command_type match {
            case DIRECTION         => ExecuteDirection
            case DIGIT             => ExecuteDigit
            case GAME_INTERACTION  => ExecuteGameInteraction
            case GAME_MANIPULATION => ExecuteGameManipulation
            case OTHER             => ExecuteOther
        }
    }

    def commandRequest (main_command: String): Unit = {
        if (status == FIRST_CALL ) {
            if (main_command != "" && body.cmdline.text != "") body.logs.text += "\n$ " + main_command
            boundTypeFun(getCommandType(main_command))(main_command)()
        } else {
            body.logs.text += "\n" + "?" + prompt + next(0) + ".ans\t<-\t" + main_command
            next(0) match {
                case "set" => { trySet (body.cmdline.text.split(" ")) }
                case "show" =>{ show (body.cmdline.text.split(" ")) }
                case "itm" => { itm (body.cmdline.text.split(" ")) }
                case _ => { body.logs.text += "\nInternal error: unexpected a status > 0 ;:)"; status = FIRST_CALL }
            }
        }
        body.cmdline.text = ""
    }

    var shortcuts: Map[Key.Value, () => Unit] = Map(
        (Key.Semicolon,  actionFromText("focus")),
        (Key.Colon  ,    actionFromText("focus")),
        (Key.Numpad0,    actionFromText("0")),
        (Key.Numpad1,    actionFromText("1")),
        (Key.Numpad2,    actionFromText("2")),
        (Key.Numpad3,    actionFromText("3")),
        (Key.Numpad4,    actionFromText("4")),
        (Key.Numpad5,    actionFromText("5")),
        (Key.Numpad6,    actionFromText("6")),
        (Key.Numpad7,    actionFromText("7")),
        (Key.Numpad8,    actionFromText("8")),
        (Key.Numpad9,    actionFromText("9")),
        (Key.Escape,     actionFromText("Escape")),
        (Key.Up,         actionFromText("Up")),
        (Key.K,          actionFromText("Up")),
        (Key.Down,       actionFromText("Down")),
        (Key.J,          actionFromText("Down")),
        (Key.Right,      actionFromText("Right")),
        (Key.L,          actionFromText("Right")),
        (Key.Left,       actionFromText("Left")),
        (Key.H,          actionFromText("Left")),
        (Key.Q,          actionFromText("quit")),
        (Key.P,          actionFromText("Space")),
        (Key.Space,      actionFromText("Space")),
        (Key.O,          actionFromText("list")),
        (Key.N,          actionFromText("step_multiple"))
    )

    var status: Status = FIRST_CALL
    var main_command: String = null

    var prompt: String = "\t>"

    // Stores args for the next function to be executed + the repetition factor
    var next = new Array[String](10)

    var repeat: Int = 1

    // handle the auto-play
    def scheduler: Scheduler = ActorSystem.create("timer-example").scheduler
    var runner: Cancellable = null


    def tryMove (dir: Direction): Unit = {
        player.move(dir)
        /* if (player.move(dir)) {
            body.logs.text += prompt + "Player goes " + dir + "\n"
            body.logs.text += "-> " + player.position.i + ", " + player.position.j + "\n"
        } else {
            body.logs.text += "\t> Player cannot go " + dir + "\n"
        } */
        room.locs.map(_.updateVisuals)
    }

    def getOrganismById (id: Int): Organism = {
        val lily : List[Organism] = body.organisms.toList
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
        for ( o <- body.organisms.toList ) {
            body.logs.text += "\n" + i + "-\t" + o
            i += 1
        }
    }

    def trySet (args: Array[String]) : Unit = {
        status match {
            case FIRST_CALL => {
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
                        case _ => { body.logs.text += "\nError: unbound value " + args(0) + " ;:("; null }
                    }
                    if (stat != null) {
                        stat.base = new_value
                        stat.syncBase
                    }
                    body.logs.text += "\n" + target_organism
                }
                case 1 => { // The command is incomplete => ask id; ask value (whatever the initial command was).
                    status = SEC_CALL // The next call will be passed to the second step
                    next(0) = "set"
                    next(1) = args(0)
                    body.logs.text += "\n" + prompt + "Which organism do you want to affect? (type l to list the organisms)"
                }
                case _ => { body.logs.text += "\nInternal error: `set` missing argument" }
                }
            }
            case SEC_CALL => {
                if(args(0) == "l") { list }
                else {
                    status = TER_CALL
                    next(2) = args(0)
                    body.logs.text += "\n" + prompt + "What is the new value of " + next(1) + "?"
                }
            }
            case TER_CALL => {
                val target_id = next(2).toInt
                val new_value = args(0).toInt
                val target_organism = getOrganismById(target_id)
                //body.logs.text += "\n\n\n" + target_organism
                val stat = next(1) match {
                        case "SPD" => { target_organism.stats.speed }
                        case "HP" =>  { target_organism.stats.health }
                        case "POW" => { target_organism.stats.power }
                        case "DEF" => { target_organism.stats.resistance }
                        case "DEC" => { target_organism.stats.decisiveness }
                        case _ => { body.logs.text += "\nError: unbound value " + args(0) + " ;:("; null }
                }
                if (stat != null) {
                    stat.base = new_value
                    stat.syncBase
                }
                body.logs.text += "\n\n\n" + target_organism
                status = FIRST_CALL
            }
            case _ => {
                body.logs.text += "\nInternal error: command.trySet entered with status > 2 ;:)"
                status = FIRST_CALL
            }
        }
    }

    def show (arg: Array[String]) : Unit = {
        status match {
            case FIRST_CALL => {
                if(arg.length == 1) {
                    body.logs.text += "\n" + getOrganismById(arg(0).toInt)
                } else {
                    status = SEC_CALL
                    next(0) = "show"
                    body.logs.text += "\nWhich organism do you want to look for ? (l to list them)"
                }
            }
            case SEC_CALL => {
                arg(0) match {
                    case "l" => {
                        list
                        body.logs.text += "\nWhich organism do you want to look for ? (l to list them)"
                    }
                    case _ => {
                        status = FIRST_CALL
                        body.logs.text += "\n" + getOrganismById(arg(0).toInt)
                    }
                }
            }
            case _ => {
                body.logs.text += "\n" + prompt + "Error, try `help` for usage"
            }
        }
    }

    def help (args: Array[String]): Unit = {
        if(args.length == 0) {
            try {
                body.logs.text += "\n"
                val src = Source.fromFile("help/help")
                src.foreach { s => body.logs.text += s }
                src.close
            } catch {
                case e: FileNotFoundException => println("Error: Help file not found")
                case e: IOException => println("Error: Failed to open help file")
            }
        } else {
            for (i <- args) {
                try {
                    body.logs.text += "\n"
                    val src = Source.fromFile("help/help." + i)
                    src.foreach { s => body.logs.text += s }
                    src.close
                } catch { case e: java.io.FileNotFoundException => body.logs.text += "Internal Error: help unavailable for `" + i + "`" }
            }
        }
    }

    def step (arg: Array[String]) : Unit = {
        // body.logs.text += "\n"
        if(arg.length == 0) { body.step }
        else {
            for(i <- 1 to (arg(0).toInt)) { body.step }
        }
    }

    def play (arg: Array[String]) : Unit = {
        body.logs.text += "\n"
        if (body.isPlaying) return
        body.isPlaying = true
        if (arg.length == 0) {
            runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(1,TimeUnit.SECONDS)) { body.step }
        } else {
            runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(arg(0).toInt,TimeUnit.SECONDS)) { body.step }
        }
    }

    def stop : Unit = {
        if(body.isPlaying) { runner.cancel(); body.isPlaying = false }
        else { () }
    }

    def repeatAction (action: () => Unit): Unit = {
        if (repeat > 1) body.logs.text += "\nRepeating " + repeat + " times the action ..."
        for (i <- 1 to repeat) action()
        if (repeat > 1) body.logs.text += "\ndone"
        repeat = 1
    }

    def itm (arg: Array[String]): Unit = {
        if(arg.length == 0) {
            status match {
                case FIRST_CALL => {
                    next(0) = "itm"
                    status = SEC_CALL
                    body.logs.text += "\nWhich action would you like to perform ?\n\tadd\n\tdel\n\tlvl => (set|(de|in)crease) the level of a given item\n\t->"
                }
                case SEC_CALL => {
                    arg(0) match {
                        case "add" => { itmadd (new Array[String](0)) }
                        case "del" => { itmdel (new Array[String](0)) }
                        case "lvl" => { itmlvl (new Array[String](0)) }
                    }
                }
            }
        } else {
            arg(0) match {
                case "add" => { itmadd(arg.tail) }
                case "del" => { itmdel(arg.tail) }
                case "lvl" => { itmlvl(arg.tail) }
                case "list"=> { itmlist(arg.tail) }
            }
        }
    }

    def itmadd (arg: Array[String]): Unit = {
        // TODO!
    }
    def itmdel (arg: Array[String]): Unit = {
        // TODO!
    }
    def itmlvl (arg: Array[String]): Unit = {
        status match {
            case FIRST_CALL => {
                arg.length match {
                    case 3 => { if(arg(0) == "set") { getItmById(arg(1).toInt).level = arg(2).toInt } else { body.logs.text += "\nError, item command" } }
                    case 2 => {
                        arg(0) match { // that makes up-set-down haha!
                            case "up" =>  { getItmById(arg(1).toInt).levelUp }
                            case "down" =>{ getItmById(arg(1).toInt).levelDown }
                            case _ =>     { body.logs.text += "\nError: `" + arg(0) + "` is not a defined command" }
                        }
                    }
                    case 1 => {
                        arg(0) match {
                            case "up" =>  { next(1) = "up"}
                            case "down" =>{ next(1) = "down" }
                            case "set" => { next(1) = "set" }
                            case _ =>     { body.logs.text += "\nError: `" + arg(0) + "` is not a defined command" }
                        }
                        next(0) = "itmlvl"
                        status = SEC_CALL
                        body.logs.text += "\nOn which items woul you like to apply these changes ? (l to list them)"
                    }
                    case 0 => {
                        body.logs.text += "\nWhat action would you like to perform ?\n\tup -> increase a level\n\tdown -> decrease a level\n\tset -> set a level\n\t=>"
                        next(0) = "itmlvl"
                        status = TER_CALL
                    }
                }
            }
            case SEC_CALL => {
                //TODO!
            }
            case TER_CALL => {
                //TODO!
            }
            case _ => {
                body.logs.text += "\nError ..."
                status = FIRST_CALL
            }
        }
    }
    def itmlist (arg: Array[String]): Unit = {
        var n: Int = 0
        body.items.foreach ( itm => {
            body.logs.text += "\nItem " + n + "\n\t" + itm
            n += 1
        })
    }
    def getItmById(i: Int): Item = {
        if(i > body.items.size) {
            return null
        } else {
            var n: Int = 0
            body.items.foreach ( itm => {
                if(n == i) { return itm }
                n += 1
            })
        }
        return null
    }
}
