import java.util.concurrent.TimeUnit
import akka.actor._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.FileNotFoundException
import java.io.IOException
import scala.io.Source
import scala.swing._
import event._

import Direction._

/*
*  The Command class defines several CommandManager inherited classes
*  | these handle a certain type of commands
*  | and keep separate history, (un|re)do lists, ...
*  The Command class interacts with the BodyPart.
*
*  A Room object is required to initiate a Command or Command Management
*  class to access logs / cmdline / items / organisms(.) / ...
*/



abstract class CommandManager (room: Room) {
    /*
    ** Appends a string to the log TextArea of room.body
    */
    def appendLogs (str: String, ln_after: Boolean = true, ln_before: Boolean = false): Unit = {
        if (ln_after && ln_before) room.body.logs.text += "\n" + str + "\n"
        else if (ln_after) room.body.logs.text += str + "\n"
        else if (ln_before) room.body.logs.text += "\n" + str
        else room.body.logs.text += str
    }



    /*
    ** useful variables
    */
    val acceptedCommands: List[String]
    var undo: List[() => Unit] = List()
    var redo: List[() => Unit] = List()     //
    var history: List[String] = List()      // History of all entered commands (composed commands are tight)
    var help_menus: List[String] = List()   // List of the help entries concerning the command handler
    var prompt:String = ">>"



    /*
    ** Definitions of the Command-interception methods
    */
    def commandSplit (command: String): Array[String] = { command.split("\\s+") }// takes a string as input and split it into an array
    def unSplitCommand(arr: Array[String]): String = {// Reverse the previous function (deletes the multi-char spaces)
        var out: String = ""
        arr.foreach(elt => out += elt + " ")
        return out
    }
    def command_syntax_check(parsed_command: Array[String], syntax:Array[String]): Boolean = {
        /**/
        return true
    }

    def acceptCommand (str: String): Boolean = {      // True if the command is handled (rq: multi-line def => com' alignment looks better)
        acceptedCommands.exists(x => x == str.split("\\s+").head)
    }

    // This function returns a string that will precede the next command of the user.
    // returning "" is equivalent to `the command is over` or `abort the current command`.
    def executeCommand (command: String): String = {
        val splited_command = commandSplit(command)
        if (splited_command.length > 1 && splited_command(1) == "help") {
            appendLogs("Help can be found running `help [.]`,\n\twhere [.] is", ln_after=false)
            help_menus.foreach ( o =>
                appendLogs("\n\t| " + o)
                )
            return "";
        } else return realExecuteCommand(splited_command)
    }
<<<<<<< HEAD
    def realExecuteCommand (splited_command: Array[String]): String
    def executeIfAcceptCommand (str: String): String = {// Checks is the command is accepted, and proceed if yes.
        if(!acceptCommand(str)) return ""
=======
    def realExecuteCommand (splited_command: Array[String]): String // empty if the execution is over (otherwise waiting for interaction)
    def executeIfAcceptCommand (str: String): String = { // empty if the command is over (or if not accepted)
        if(acceptCommand(str)) return ""
>>>>>>> origin/master
        else return executeCommand(str)
    }
}



// The following  class deals with the movements of the player
class DirectionsCommand(room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("Up", "Down", "Left", "Right")
    help_menus = "directions" :: Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        splited_command(0) match {
            case "Up"    => { room.body.player.move(UP)    }
            case "Down"  => { room.body.player.move(DOWN)  }
            case "Left"  => { room.body.player.move(LEFT)  }
            case "Right" => { room.body.player.move(RIGHT) }
            case _       => { appendLogs("Error: Direction `" + splited_command(0) + "` unknown") }
        }
        room.locs.map(_.updateVisuals)// Update the map that the user sees.
        return ""
    }
}



// The following class deals withe the repeat variable (allowing to advance by `repeat` steps the state of the game).
class DigitsCommand(room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "+", "-", "repeat-reset")
    help_menus = "repeat" :: Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        splited_command(0) match {
            case "0"            => { room.body.repeat *= 10; return "" }
            case "1"            => { room.body.repeat = room.body.repeat * 10 + 1; return "" }
            case "2"            => { room.body.repeat = room.body.repeat * 10 + 2; return "" }
            case "3"            => { room.body.repeat = room.body.repeat * 10 + 3; return "" }
            case "4"            => { room.body.repeat = room.body.repeat * 10 + 4; return "" }
            case "5"            => { room.body.repeat = room.body.repeat * 10 + 5; return "" }
            case "6"            => { room.body.repeat = room.body.repeat * 10 + 6; return "" }
            case "7"            => { room.body.repeat = room.body.repeat * 10 + 7; return "" }
            case "8"            => { room.body.repeat = room.body.repeat * 10 + 8; return "" }
            case "9"            => { room.body.repeat = room.body.repeat * 10 + 9; return "" }
            case "+"            => { room.body.repeat += 1; return "" }
            case "-"            => { room.body.repeat -= 1; return "" }
            case "repeat-reset" => { room.body.repeat = 1; return "" }
            case "repeat"       => { if (splited_command.length == 1) room.body.repeat = 1 else room.body.repeat = splited_command(1).toInt; return "" }
            case _       => { appendLogs("Error: Command `" + splited_command(0) + "` unknown") }
        }
        return ""
    }
}



// The following class is required to select organisms.
class SelectionCommand (room: Room) extends CommandManager (room) {
<<<<<<< HEAD
    val acceptedCommands: List[String] = List("select", "take", "filter", "flush", "selection-print", "selection")
    help_menus = "select" :: "selection" :: "take" :: "filter" :: "flush" :: Nil
=======
    val acceptedCommands: List[String] = List("select", "take", "filter", "flush", "selection-print")
    help_menus = Nil
>>>>>>> origin/master

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        var splited_command: Array[String] = splited_command_arg// necessary because the arguments are non mutable variables
        def selection_select: String = {
            splited_command.length match {
                case 1 => {
                    appendLogs("What kind of selection do you want to make?\n\t1-> rectangle\n\t\t| the top-left corner or bottom-right corner will be asked\n\t2-> circle\n\t\t| the center and a point on the perimeter will be asked")
                    return "select"
                }
                case 3 | 5 => {
                    appendLogs ("Illegal number of parameters (plausible error: coordinates must be passed as `i j`, not `i`, `j`.)\n\tAborting.")
                    return ""
                }
                case 2 => {
                    appendLogs ("What is the first cell to mark? (click on it or write \"`i` `j`\" in the command line.)")
                    return unSplitCommand(splited_command)
                }
                case 4 => {
                    appendLogs ("What is the second cell to mark? (click on it or write \"`i` `j`\" in the command line.)")
                    return unSplitCommand(splited_command)
                }
                case 6 => {
                    val x1: Int = splited_command(2).toInt
                    val y1: Int = splited_command(3).toInt
                    val x2: Int = splited_command(4).toInt
                    val y2: Int = splited_command(5).toInt
                    room.body.organisms_selection = room.body.organisms_selection.empty
                    splited_command(1) match {
                        case "1" | "rectangle" | "rect" => {// rectangular selection
                            for(i <- x1 to x2) {
                                for (j <- y1 to y2) {
                                    room.body.organisms_selection ++= room.locs(i, j).organisms(0)
                                    room.body.organisms_selection ++= room.locs(i, j).organisms(1)
                                }
                            }
                        }
                        case "2" | "circle" | "circ"       => {// circular selection
                            val R2: Int = (x1 - x2)^2 + (y1 - y2)^2
                            val R: Int = scala.math.sqrt(R2).ceil.toInt
                            for(i <- x1 - R to x1 + R) {
                                for (j <- y1 - R to y1 + R) {
                                    if (0 <= i && i < room.rows && 0 <= j && j < room.cols && ((x1 - i )^2 + (y1 - j)^2) <= R2 ) {
                                        room.body.organisms_selection ++= room.locs(i, j).organisms(0)
                                        room.body.organisms_selection ++= room.locs(i, j).organisms(1)
                                    }
                                }
                            }
                        }
                        case _ => { appendLogs("Internal error: unknown selection type.") }
                    }
                    appendLogs("Selection complete: " + room.body.organisms_selection.size + " elements\n\nUse `selection-print` to print the selection")
                    return ""
                }
                case _ => {
                    appendLogs("Too much arguments... Aborting. :/")
                    return ""
                }
            }
        }

        def selection_take: String = {// take every items of the current selected organisms
            room.body.organisms_selection.foreach(o => {
                if(o.isFriendly) {
                    room.body.player.inventory ++= o.items
                    o.items.empty
                }
            })
            appendLogs("The player has stolen the items of the selected friendly organisms")
            return ""
        }

        def selection_filter: String = {// only keep friendly or unfriendly organisms in the current selection
            splited_command.length match {
                case 1 => {
                    appendLogs("What family of organism do you want to keep ? (virus|cell)")
                    return "filter"
                }
                case 2 => {
                    if (splited_command(1) == "cell") room.body.organisms_selection = room.body.organisms_selection.filter ( o => !o.isFriendly )
                    else room.body.organisms_selection = room.body.organisms_selection.filter ( o => o.isFriendly )
                    appendLogs("Filter applied to the selection.")
                }
                case _ => { appendLogs("Illegal number of arguments") }
            }
            return ""
        }

        def selection_print: String = {// Prints the current selection
            appendLogs("   ---   Printing " + room.body.organisms_selection.size + " elements:   ---")
            room.body.organisms_selection.foreach ( o => appendLogs("" + o) )
            appendLogs("   ---   End of the selection   ---", ln_before = true)
            return ""
        }

<<<<<<< HEAD
        def selection_selection: String = {// Hub to decide which function is to be executed.
            splited_command.length match {
                case 1 => {
                    appendLogs("Wrong usage of command `selection`\n\t`-> check `help selection` to find out why :)")
                    return ""
                }
                case _ => {
                    splited_command(1) match {
                        case "select" => {
                            splited_command = splited_command.tail
                            return selection_select
                        }
                        case "take"   => return selection_take
                        case "filter" => {
                            splited_command = splited_command.tail
                            return selection_select
                        }
                        case "flush"  => { room.body.organisms_selection --= room.body.organisms_selection; return "" }
                        case "print"  => return selection_print
                        case _        => {
                            appendLogs("Error: command `" + splited_command(1) + "` unknown. Aborting.")
                            return ""
                        }
                    }
                }
            }
            return ""
        }

        splited_command(0) match {// main switch to know what function corresponds to the command at hand.
            case "selection"        => { return selection_selection }
            case "select"           => { return selection_select }
            case "take"             => { return selection_take }
            case "filter"           => { return selection_filter }
            case "flush"            => { room.body.organisms_selection --= room.body.organisms_selection; return "" }
            case "selection-print"  => { return selection_print }
            case _                  => { appendLogs("Error: Command `" + splited_command(0) + "` unknown") }
=======
        splited_command(0) match {
            case "select" => { return selection_select }
            case "take"   => { return selection_take }
            case "filter" => { return selection_filter }
            case "flush"  => { room.body.organisms_selection --= room.body.organisms_selection; return "" }
            case "selection-print"  => { return selection_print }
            case _        => { appendLogs("Error: Command `" + splited_command(0) + "` unknown") }
>>>>>>> origin/master
        }
        return ""
    }
}


class BehaviorCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("behavior", "behavior-cursor", "behavior-target")
    help_menus = "behavior" :: Nil
    import Behavior._

    def realExecuteCommand (splited_command: Array[String]): String = {
        def behavior: String = {
            splited_command.length match {
                case 1 => {
                    appendLogs("Where should they go ? (1 -> cursor, 2 -> click target)")
                    return "behavior"
                }
                case 2 => {
                    return splited_command(1) match {
                        case "cursor" | "1" => "behavior-cursor"
                        case "target" | "2" | "click" => "behavior-target"
                        case _ => { appendLogs("Error: no such behavior"); "" }
                    }
                }
                case _ => {
                    appendLogs("Error: too many parameters; see `help behavior`\nAborting.")
                    return ""
                }
            }
        }

        def behavior_cursor: String = {
            room.body.organisms_selection.foreach(o => {
                if (o.isFriendly) {
                    o.behavior = { () => (room.body.player.position, SEEK) }
                }
            })
            appendLogs("The friendly organisms have changed their target")
            return ""
        }

        def behavior_target: String = {
            splited_command.length match {
                case 1 => {
                    appendLogs("Which tile ?")
                    return "behavior-target"
                }
                case 2 => {
                    appendLogs ("Illegal number of parameters (plausible error: coordinates must be passed as `i j`, not `i`, `j`.)\n\tAborting.")
                    return ""
                }
                case 3 => {
                    val i = splited_command(1).toInt
                    val j = splited_command(2).toInt
                    room.body.organisms_selection.foreach(o => {
                        if (o.isFriendly) {
                            o.behavior = { () => (room.locs(i, j), SEEK) }
                        }
                    })
                    appendLogs("The friendly organisms have changed their target")
                    return ""
                }
                case _ => { appendLogs("Illegal number of arguments. Aborting.") }
            }
            return ""
        }

        splited_command(0) match {
            case "behavior" => { return behavior }
            case "behavior-cursor" => { return behavior_cursor }
            case "behavior-target" => { return behavior_target }
            case _ => { appendLogs("Error: Command `" + splited_command(0) + "` unknown") }
        }
        return ""
    }
}


// The following class deals with the management of the organisms
class OrganismsCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("list", "set", "show")
    var last_checked_arg: Int = 0
    help_menus = "list" :: "set" :: "show" :: Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        def getOrganismById (id: Int): Organism = {
            val lily : List[Organism] = room.body.organisms.toList
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

        def organisms_list: Unit = {
            appendLogs("   ---   Printing " + room.body.organisms.size + " organisms:   ---")
            var i: Int = 0
            for ( o <- room.body.organisms.toList ) {
                appendLogs(i + "-\t" + o)
                i += 1
            }
            appendLogs("   ---   End of the list.   ---", ln_before = true)
        }

        def organisms_set: String = {// Allows the user to set a stat field of any organism to any integer value.
            last_checked_arg match {// last_checked_arg defines the last approved argument for the function.
                case 0 => {
                    splited_command.length match {
                        case 1 => {
                            appendLogs("Which organism would you like to consider? (l to list available organisms)")
                            return "set"
                        }
                        case 2 => {
                            if(splited_command(1) == "l") {
                                organisms_list
                                appendLogs("Which organism would you like to consider? (l to list available organisms)")
                                return "set"
                            } else {
                                appendLogs("Which field would you like to set? (SPD, HP, POW, DEF, DEC)")
                                last_checked_arg += 1
                                return ("set " + splited_command(1))
                            }
                        }
                        case _ => {
                            appendLogs("Interactive mode only for now :/")
                            return ("set " + splited_command(1))
                        }
                    }
                }
                case 1 => {
                    if(splited_command.length == 2) {
                        appendLogs ("Which field would you like to set? (SPD, HP, POW, DEF, DEC)")
                        return("set " + splited_command(1))
                    } else {
                        appendLogs ("What is the target value of the field " + splited_command(2) + " for the organism " + splited_command(1) + "?")
                        last_checked_arg += 1
                        return ( "set " + splited_command(1) + " " + splited_command(2) )
                    }
                }
                case 2 => {
                    val target_organism = getOrganismById(splited_command(1).toInt)
                    val target_field    = splited_command(2)
                    val target_value    = splited_command(3).toInt
                    val stat: Stat = target_field match {
                        case "SPD" => { target_organism.stats.speed }
                        case "HP" =>  { target_organism.stats.health }
                        case "POW" => { target_organism.stats.power }
                        case "DEF" => { target_organism.stats.resistance }
                        case "DEC" => { target_organism.stats.decisiveness }
                        case _ =>     { appendLogs ("Error: unknown field `" + target_field + "`"); null }
                    }
                    if(stat != null) {
                        stat.base = target_value
                        stat.syncBase
                    }
                    appendLogs(target_organism + "")
                    last_checked_arg = 0
                    return ""
                }
            }
        }

        def organisms_show: String = {// Shows the stats field of a particular organism.
            splited_command.length match {
                case 1 => {
                    appendLogs("Which organism would you like to see ?")
                    return "show"
                }
                case 2 => {
                    appendLogs("" + getOrganismById(splited_command(1).toInt))
                    return ""
                }
                case _ => {
                    appendLogs ("Too many arguments, see `list` to list several organisms")
                    return ""
                }
            }
        }

        splited_command(0) match {// main switch to defines the function that corresponds to the command at hand.
            case "list"     => { organisms_list; return ""}
            case "set"      => { return organisms_set     }
            case "show"     => { return organisms_show    }
            case _          => { appendLogs("Error: Command `" + splited_command(0) + "` unknown"); return "" }
        }
    }
}



// The following class deals with the items management
class ItemsCommand (room: Room) extends CommandManager (room) {
<<<<<<< HEAD
    val acceptedCommands: List[String] = List("item_add", "item", "item_rm", "item_pickup", "item_level", "item_list", "item_give")
=======

    val acceptedCommands: List[String] = List("item-add", "item", "item-rm", "item-pickup", "item-level", "item-list")
>>>>>>> origin/master
    help_menus = "item" :: Nil

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        var splited_command: Array[String] = splited_command_arg// necessary as the arguments of a function are non mutable
        def items_item: String = {// main redirection command to defines equivalence in the call of a specific function (eg.: item list <-> item_list)
            splited_command.length match {
                case 1 => { appendLogs("Wrong usage of command `item`\n\t`-> check `help item` to find out :)"); return "" }
                case _ => {
                    splited_command(1) match {
<<<<<<< HEAD
                        case "add"    => {
                            splited_command = splited_command.tail
                            splited_command(0) = "item_add"
                            return items_add
                        }
                        case "rm"     => {
                            splited_command = splited_command.tail
                            splited_command(0) = "item_rm"
                            return items_rm
                        }
                        case "pickup" =>  {
                            splited_command = splited_command.tail
                            splited_command(0) = "item_pickup"
                            return items_pickUp
                        }
                        case "level"  => {
                            splited_command = splited_command.tail
                            splited_command(0) = "item_level"
                            return items_level
                        }
=======
                        case "add"    => { appendLogs("What kind of item do you want to add ?\n\t1 -> Knife\n\t2-> Alcool\n\t3 -> Move\n\t4 -> Javel\n\t5-> heat\n\t6-> spike\n\t7-> leak\n\t8-> membrane"); return "item-add" }
                        case "rm"     => return "item-rm"
                        case "pickup" => return "item-pickup"
                        case "level"  => return "item-level"
>>>>>>> origin/master
                        case "list"   => return items_list
                        case "give"   => {
                            splited_command = splited_command.tail
                            splited_command(0) = "item_give"
                            return items_give
                        }
                        case _        => { appendLogs("Error: command `" + splited_command(1) + "` unknown"); return "" }
                    }
                }
            }
        }

        def items_list: String = {// List the current list of items on the board
            var i: Int = 0
            appendLogs("   ---------   Printing " + room.body.items.size +  " elements   ---------")
            for ( o <- room.body.items.toList ) {
                appendLogs(i + "-\t" + o)
                i += 1
            }
            appendLogs("   ---------   End of the printing   ---------")
            return ""
        }

        def getItemById (id: Int): Item = {
            val lily : List[Item] = room.body.items.toList
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

        def items_add: String = {// Add an item to the board.
            splited_command.length match {
                case 1 => { appendLogs("What kind of item do you want to add ?\n\t1 -> Knife\n\t2-> Alcohol\n\t3 -> Move\n\t4 -> Javel\n\t5-> heat\n\t6-> spike\n\t7-> leak\n\t8-> membrane"); return "item-add" }
                case 2 => { appendLogs("Where do you want to spawn the new item? (click or respond by `i j` in the cmdline)."); return "item-add " + splited_command(1) }
                case 3 => { appendLogs("Syntax error, chack `help` for more details"); return "" }
                case 4 => {
                    val i: Int = splited_command(2).toInt
                    val j: Int = splited_command(3).toInt
                    val new_itm = splited_command(1) match {
                        case "1" => new Knife(room.locs(i, j))
                        case "2" => new Alcohol(room.locs(i, j))
                        case "3" => new BodyMovement(room.locs(i, j))
                        case "4" => new Javel(room.locs(i, j))
                        case "5" => new Heat(room.locs(i, j))
                        case "6" => new Spike(room.locs(i, j))
                        case "7" => new CytoplasmLeak(room.locs(i, j))
                        case "8" => new MembraneReplacement(room.locs(i, j))
                    }
                    new_itm.drop
                    ""
                }
            }
        }

        def items_rm: String = {// Remove an item from the board
            splited_command.length match {
<<<<<<< HEAD
                case 1 => { appendLogs("What item do you want to remove from the game? (l to list them)"); "item_rm" }
                case _ => {
                    if (splited_command(1) == "l") {
                        items_list
                        return unSplitCommand(splited_command)
                    } else {
                        room.body.items -= getItemById(splited_command(1).toInt)
                        return ""
                    }
                }
=======
                case 1 => { appendLogs("What item do you want to remove from the game? (l to list them)"); "item-rm" }
                case _ => { room.body.items -= getItemById(splited_command(1).toInt); return "" }
>>>>>>> origin/master
            }
        }

        def items_pickUp: String = {// Force each organism to pick up an item from the board
            splited_command.length match {
                case 1 => { appendLogs("Which cell is this about? (any one-word answer for all)"); return "item-pickup" }
                case 2 => {
                    for(i <- 0 to room.cols - 1) {
                        for(j<- 0 to room.rows - 1) {
                            room.locs(i, j).organisms(0).foreach ( o =>
                                if (room.locs(splited_command(1).toInt, splited_command(2).toInt).items.size > 0) {
                                    val it = room.locs(splited_command(1).toInt, splited_command(2).toInt).items.head
                                    if (it.pickUp(o)) {
                                        o.items += it
                                        room.body.logs.text += "\nI " + o + " pick up the item, yay !"
                                    }
                                }
                            )
                        }
                    }
                }
                case 3 => {
                    room.locs(splited_command(1).toInt, splited_command(2).toInt).organisms(0).foreach ( o =>
                        if (room.locs(splited_command(1).toInt, splited_command(2).toInt).items.size > 0) {
                            val it = room.locs(splited_command(1).toInt, splited_command(2).toInt).items.head
                            if (it.pickUp(o)) {
                                o.items += it
                                room.body.logs.text += "\nI " + o + " pick up the item, yay !"
                            }
                        }
                    )
                    return ""
                }
            }
            ""
        }

        def list_inventory: Unit = {// List the inventory of the player
            var i: Int = 0
            appendLogs("   ---------   Printing " + room.body.player.inventory.size +  " elements   ---------")
            for ( o <- room.body.player.inventory.toList ) {
                appendLogs(i + "-\t" + o)
                i += 1
            }
            appendLogs("   ---------   End of the printing   ---------")
        }

        def getItemFromInventoryById (id: Int): Item = {
            var i: Int = 0
            for ( o <- room.body.player.inventory.toList ) {
                if(i == id) return o
                i += 1
            }
            return null
        }

        def getOrganismById (id: Int): Organism = {
            val lily : List[Organism] = room.body.organisms.toList
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

        def organisms_list: Unit = {
            appendLogs("   ---   Printing " + room.body.organisms.size + " organisms:   ---")
            var i: Int = 0
            for ( o <- room.body.organisms.toList ) {
                appendLogs(i + "-\t" + o)
                i += 1
            }
            appendLogs("   ---   End of the list.   ---", ln_before = true)
        }

        def items_give: String = {// Give an item from the inventory to an organism
            splited_command.length match {
                case 1 => {
                    appendLogs ("Which item of the inventory would you like to give? (`l` to list the items)")
                    return "item_give"
                }
                case 2 => {
                    if ( splited_command(1) == "l") {
                        list_inventory
                        appendLogs ("Which item of the inventory would you like to give? (`l` to list the items)")
                        return "item_give"
                    } else {
                        appendLogs ("What organism should revieve the item? (`l` to list the organisms)")
                        return unSplitCommand(splited_command)
                    }
                }
                case 3 => {
                    if(splited_command(2) == "l") {
                        organisms_list
                        appendLogs ("What organism should revieve the item? (`l` to list the organisms)")
                        return unSplitCommand(splited_command)
                    } else {
                        val target_item: Item = getItemFromInventoryById(splited_command(1).toInt)
                        val target_organism: Organism = getOrganismById(splited_command(2).toInt)
                        room.body.player.inventory -= target_item
                        target_organism.items += target_item
                        appendLogs ( "The organism " + splited_command(2) + " revieved the item " + splited_command(1))
                        return ""
                    }
                }
                case _ => {
                    appendLogs("Too many arguments for the command `items_give`\n\tAborting.")
                    return ""
                }
            }
        }

        def items_level: String = {
            /*TODO! */""
        }

<<<<<<< HEAD
        splited_command(0) match {// main switch to defines the function which corresponds to the command at hand.
            case "item_add"    => { return items_add    }
            case "item_rm"     => { return items_rm     }
            case "item_pickup" => { return items_pickUp }
            case "item_level"  => { return items_level  }
            case "item_list"   => { return items_list   }
            case "item_give"   => { return items_give   }
=======
        splited_command(0) match {
            case "item-add"    => { return items_add    }
            case "item-rm"     => { return items_rm     }
            case "item-pickup" => { return items_pickUp }
            case "item-level"  => { return items_level  }
            case "item-list"   => { return items_list   }
>>>>>>> origin/master
            case "item"        => { return items_item   }
            case _             => { appendLogs("Error: Command `" + splited_command(0) + "` unknown"); return "" }
        }
        return ""
    }
}



class OtherCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("quit", "toggle", "stop", "play", "step", "step-multiple", "focus-cmdline", "focus-win", "q", "clear")
    help_menus = Nil

    // The two following variables are used to make an automatic and regular step in the game evolution.
    def scheduler: Scheduler = ActorSystem.create("timer-example").scheduler
    var runner: Cancellable = null

    def realExecuteCommand (splited_command: Array[String]): String = {
        def other_play: String = {// start the automatic step by step
            if (room.body.isPlaying) return ""
            room.body.isPlaying = true
            if (splited_command.length == 1) {
                runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(1,TimeUnit.SECONDS)) { room.body.step }
            } else {
                runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(splited_command(1).toInt,TimeUnit.SECONDS)) { room.body.step }
            }
            return ""
        }
        def other_stop : String = {// stop the automatic step by step.
            if(room.body.isPlaying) { runner.cancel(); room.body.isPlaying = false; "" }
            else { "" }
        }
        def other_toggle : String = {// toggle the step by step.
            if(room.body.isPlaying) { other_stop }
            else { other_play }
        }
        def other_step: String = {// make a/n step
            if(splited_command.length == 1) { room.body.step }
            else {
                for(i <- 1 to (splited_command(1).toInt)) { room.body.step }
            }
            return ""
        }

        splited_command(0) match {// main switch to define which function fits with the command at hand.
            case "play"          => { return other_play }
            case "stop"          => { return other_stop }
            case "toggle"        => { return other_toggle }
            case "quit"          => { realExecuteCommand(Array[String]("stop")) ; Runtime.getRuntime().halt(0) }
            case "step"          => { return other_step }
            case "step-multiple" => { realExecuteCommand(Array[String]("step", room.body.repeat.toString)); room.body.repeat = 1; return "" }
            case "focus-cmdline" => { room.body.cmdline.requestFocusInWindow(); return "" }
            case "focus-win"     => { room.body.globalPanel.requestFocusInWindow(); return "" }
            case "q"             => { room.body.globalPanel.requestFocusInWindow(); return "" }
            case "clear"         => { room.body.logs.text = ""; return "" }
            case _               => { appendLogs("Error: Command `" + splited_command(0) + "` unknown"); return "" }
        }
        return ""
    }
}



class HelpCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("help")
    help_menus = Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        var buffer: String = "\n"// Using a buffer is much faster that adding each character at a time to the current room.body.logs
        if(splited_command.length == 1) {
            try {
                val src = Source.fromFile("help/help")
                src.foreach { s => buffer += s }
                src.close
                appendLogs(buffer)
            } catch {
                case e: FileNotFoundException => println("Error: Help file not found")
                case e: IOException => println("Error: Failed to open help file")
            }
        } else {
            for (i <- 1 to  splited_command.length - 1) {
                try {
                    appendLogs ("Reading help from `help/" + splited_command(i) + ".help`", ln_after=true)
                    val src = Source.fromFile("help/" + splited_command(i) + ".help")
                    src.foreach { s => buffer += s }
                    src.close
                    appendLogs(buffer)
                } catch { case e: java.io.FileNotFoundException => appendLogs ("\nInternal Error: help unavailable for `" + splited_command(i) + "`") }
            }
        }
        return ""
    }
}

// accept every command and do nothing.
class NullCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List()
    override def acceptCommand (str: String): Boolean = { true }
    help_menus = Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        appendLogs("Error: Command `" + splited_command(0) + "` unknown")
        return ""
    }
}



// --------- main command manager ---------



class Command (val room: Room) {
    def appendLogs (
            str: String,
            ln_after: Boolean = true,
            ln_before: Boolean = false
            ): Unit = {
        if (ln_after && ln_before) room.body.logs.text += "\n" + str + "\n"
        else if (ln_after) room.body.logs.text += str + "\n"
        else if (ln_before) room.body.logs.text += "\n" + str
        else room.body.logs.text += str
    }

    // defines the active command at any given time.
    var current_command: String = ""

<<<<<<< HEAD
    val bind_keys: Map[Key.Value, String] = Map(// defines the current key-bindings for the app.
        (Key.Semicolon,  "focus_cmdline"),
        (Key.Colon  ,    "focus_cmdline"),
=======
    val bind_keys: Map[Key.Value, String] = Map(
        (Key.Semicolon,  "focus-cmdline"),
        (Key.Colon  ,    "focus-cmdline"),
>>>>>>> origin/master
        (Key.Numpad0,    "0"),
        (Key.Numpad1,    "1"),
        (Key.Numpad2,    "2"),
        (Key.Numpad3,    "3"),
        (Key.Numpad4,    "4"),
        (Key.Numpad5,    "5"),
        (Key.Numpad6,    "6"),
        (Key.Numpad7,    "7"),
        (Key.Numpad8,    "8"),
        (Key.Numpad9,    "9"),
        (Key.Escape,     "repeat-reset"),
        (Key.Up,         "Up"),
        (Key.K,          "Up"),
        (Key.Down,       "Down"),
        (Key.J,          "Down"),
        (Key.Right,      "Right"),
        (Key.L,          "Right"),
        (Key.Left,       "Left"),
        (Key.H,          "Left"),
        (Key.Q,          "quit"),
        (Key.P,          "play"),
        (Key.S,          "stop"),
        (Key.Space,      "toggle"),
        (Key.O,          "list"),
        (Key.N,          "step-multiple"),
        (Key.Enter,      "click-cell")
    )
    var aliases: Map[String, String] = Map ()// defines aliases (not used yet)

    // creates the different classes to deal with commands
    val direction_command = new DirectionsCommand(room)
    val digits_command    = new DigitsCommand(room)
    val selection_command = new SelectionCommand(room)
    val organisms_command = new OrganismsCommand(room)
    val items_command     = new ItemsCommand(room)
    val behavior_command  = new BehaviorCommand(room)
    val help_command      = new HelpCommand(room)
    val other_command     = new OtherCommand(room)
    val null_command      = new NullCommand(room)


    def commandFromKey (key: Key.Value) : String = {   // Get a string command from the use of a key
        try bind_keys(key)
        catch { case e: java.util.NoSuchElementException => { "" } }
    }

    def locsClicked ( p: Pos ): Unit = {
        if (current_command == "") {// no current command -> display the location's content
            appendLogs (p.listContents)
        } else {// there is a current command, append the coordinates of the location to the command line.
            room.body.cmdline.text += " " + p.i + " " + p.j
        }
    }

    def keyPressed (c: Key.Value): Unit = {// Called when the user presses a key outside of the cmdline TextArea.
        commandRequest(commandFromKey(c))
    }

    def unSplitCommand(arr: Array[String]): String = {
        var out: String = ""
        arr.foreach(elt => out += elt + " ")
        return out
    }

    def commandRequest(command: String): Unit = {
        if (command.split("\\s+").length == 0) {// if the command is only composed of spaces, ignore it.
            current_command = ""
            room.body.cmdline.text = ""
            return
        }
        else if (command.split("\\s+").head == "abort") {// if the line starts with abort, abort the current command.
            appendLogs("Aborting ...")
            current_command = ""
            room.body.cmdline.text = ""
            return
        } else if (command == "click_cell") {// fake a click on the current location of the player
            locsClicked (room.body.player.position)
        } else {
            if(current_command != "") current_command += " " + command
            else current_command = command
        }
        //appendLogs("current := `" + current_command + "`")
        var toBeExecuted: (String => String) = List(
            direction_command,
            digits_command,
            selection_command,
            organisms_command,
            items_command,
            behavior_command,
            help_command,
            other_command,
            null_command,
        ).filter(_.acceptCommand(current_command))
        .head // never fails since the filtered list contains at least null_command which accepts everything
        .executeCommand

        current_command = toBeExecuted(current_command)
        room.body.cmdline.text = ""
    }
}

// Une petite ligne pour une sœur disparue: `boundTypeFun(getCommandType(main_command.split(" ")(0)))(main_command)()`
