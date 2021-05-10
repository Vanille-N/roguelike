import scala.swing._
import event._

import Direction._

case class PrintInLogs (str: String, ln_after: Boolean = true, ln_before: Boolean = false) extends Event
case class SendCommandToServer (player_id: Int, command: String) extends Event
case class ClearLogs() extends Event

/*
*  The Command class defines several CommandManager inherited classes
*  | these handle a certain type of commands
*  | and keep separate history, (un|re)do lists, ...
*  The Command class interacts with the BodyPart.
*
*  A Room object is required to initiate a Command or Command Management
*  class to access logs / cmdline / items / organisms(.) / ...
*/



abstract class ClientCommandManager (val body: BodyPart, val game: Game)
extends Publisher {
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
    def command_syntax_check(parsed_command: Array[String], syntax:Array[Tuple2[Boolean, String]]): Boolean = {// This function checks whether a command's syntax is correct or not
        /*
        ** The tuples are of the form (b: Boolean, s: String) with:
        **  | b: Boolean -> Can the command stop here (temporarily)
        **  | s: String ->  type  of the argument.
        **              | "[abc]" denotes the letters 'a', 'b' or 'c' (any word in L((a + b + c)*) is accepted)
        **              | "'abc" denotes the word "abc"
        **              | "N" denotes any integer
        **              | "N:n->m;" denotes any integer between the integer n and the integer m (numerical values, no variables allowed)
        **              | it is possible to mix these the options with '|'.
        **                  | e.g.: have "[l]|N" to denote either the letter 'l', or any number.
        **              | "any" denotes an accepted word.
        */

        // 1- Checking the global form of the command.
        if (parsed_command.length > syntax.length) {// the length of a command should not exceed the maximal size of the described command.
            publish(PrintInLogs("_/!\\_: The command exceeds the maximal length."))
            return false
        }
        if ( !(syntax(parsed_command.length - 1)._1) ) {// the command should not stop where it did :/
            publish(PrintInLogs("_/!\\_: The command should not end on this parameter."))
            return false
        }

        var acceptable: Array[String] = Array()// stores the different expressions of the form "[.]", "'.", "N" or "N:.->.;" separated by '|'
        var reason_of_withdraw: String = "Possibilities:"
        // 2- Checking each parameter / field.
        for (i <- 0 to parsed_command.length - 1) {
            acceptable = (syntax(i)._2).split("\\|")
            //println("\ni = " + i + "\tacceptable.length = " +  acceptable.length)
            reason_of_withdraw = ""
            if ( !acceptable.exists (
                elt => {// elt is a string whose the form is described above.
                    var result: Boolean = true
                    elt(0) match {
                        case '[' => {
                            var acceptable_letters: Set[Char] = Set()
                            elt.substring(1,elt.length - 1).foreach(letter => acceptable_letters += letter)
                            parsed_command(i).foreach(l => result = result && acceptable_letters.contains(l))
                            reason_of_withdraw += "\n\t| word composed of the following letters: " + acceptable_letters.toString.substring(4, acceptable_letters.toString.length - 1)
                        }
                        case 'N' => {
                            //println("\tChecking for a number.")
                            // First, check that we have a number
                            result = parsed_command(i).forall ( letter => List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9').contains(letter) )

                            //if(result) println("\t\tI have a number.")

                            // Then, if there are boundaries, check them.
                            if(result && elt.length > 1  && elt(1) == ':') {
                                val minimum: Int = elt.substring ( 2, elt.indexOf("->") ).toInt
                                val maximum: Int = elt.substring ( elt.indexOf("->") + 2, elt.length - 1 ).toInt
                                var n: Int = 0// stores the value of the eventual number.
                                parsed_command(i).foreach ( letter => {
                                    n *= 10
                                    n += letter.toString.toInt
                                })
                                result = n >= minimum && n <= maximum
                                //if(result) println("\t\t\t... in the correct range")
                                reason_of_withdraw += "\n\t| number (between " + minimum + " and " + maximum + " )"
                            } else {
                                reason_of_withdraw += "\n\t| number"
                            }
                        }
                        case _ => {
                            //println("\tChecking for a word:")
                            result = parsed_command(i) == elt || elt == "any"
                            //if(result) println("\t\tOk: " + elt + " == " + parsed_command(i))
                            //else println("\t\tnOk: " + elt + " != " + parsed_command(i))
                            reason_of_withdraw += "\n\t| word `" + elt + "`"
                        }
                    }
                    result
                }
                )) {
                publish(PrintInLogs(s"The ${i+1} ${(i+1) match { case 1 => "-st" case 2 => "-nd" case 3 => "-rd" case _ => "-th"} }  argument does not respect its form: $reason_of_withdraw"))
                return false
            }
        }
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
            publish(PrintInLogs("Help can be found running `help [.]`,\n\twhere [.] is", ln_after=false))
            help_menus.foreach ( o =>
                publish(PrintInLogs("\n\t| " + o))
                )
            return "";
        } else return realExecuteCommand(splited_command)
    }
    def realExecuteCommand (splited_command: Array[String]): String // empty if the execution is over (otherwise waiting for interaction)
    def executeIfAcceptCommand (str: String): String = { // empty if the command is over (or if not accepted)
        if(!acceptCommand(str)) return ""
        else return executeCommand(str)
    }
}

abstract class ServerCommandManager (body: BodyPart, game: Game)
extends Publisher {
    /*
    ** When a server recieves a command, its syntax is already defined as
    ** correct. Only the execution remains.
    */
    var prompt:String = ">>"

    def unSplitCommand(arr: Array[String]): String = {// Reverse the previous function (deletes the multi-char spaces)
        var out: String = ""
        arr.foreach(elt => out += elt + " ")
        return out
    }

    def execute (splited_command: Array[String]) : Boolean
}





// --------- main command manager ---------


class Command (body: BodyPart, game: Game) extends Publisher {
    // defines the active command at any given time.
    var current_command: String = ""

    var aliases: Map[String, String] = Map ()// defines aliases (not used yet)

    // creates the different classes to deal with commands
    val subCommands: List[ClientCommandManager] = List(
        new DirectionsCommand(body, game),
        new DigitsCommand(body, game),
        new SelectionCommand(body, game),
        new OrganismsCommand(body, game),
        new ItemsCommand(body, game),
        new BehaviorCommand(body, game),
        new ArtefactsCommand(body, game),
        new LevelCommand(body, game),
        new HelpCommand(body, game),
        new OtherCommand(body, game),
        new NullCommand(body, game)
    )


    //def locsClicked ( p: Pos ): Unit = {
    //    if (current_command == "") {// no current command -> display the location's content
    //        publish(PrintInLogs(p.listContents))
    //    } else {// there is a current command, append the coordinates of the location to the command line.
    //        game.cmdline.text += " " + p.i + " " + p.j
    //    }
    //}

    def unSplitCommand(arr: Array[String]): String = {
        var out: String = ""
        arr.foreach(elt => out += elt + " ")
        return out
    }

    def commandRequest(command: String): Unit = {
        if (command == "" || command.split("\\s+").length == 0) {// if the command is only composed of spaces, ignore it.
            current_command = ""
            //publish(ClearLogs())
            return
        }
        else if (command.split("\\s+").head == "abort") {// if the line starts with abort, abort the current command.
            publish(PrintInLogs("Aborting ..."))
            current_command = ""
            publish(ClearLogs())
            return
        } else {
            if(current_command != "") current_command += " " + command
            else current_command = command
        }
        //appendLogs("current := `" + current_command + "`")
        var toBeExecuted: (String => String) =
            subCommands
            .filter(_.acceptCommand(current_command))
            .head // never fails since the filtered list contains at least null_command which accepts everything
            .executeCommand

        current_command = toBeExecuted(current_command)
        publish(ClearLogs())
    }
}

