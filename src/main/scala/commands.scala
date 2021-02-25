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

object Status extends Enumeration {
    type Status = Value
    val FIRST_CALL = Value("first call")
    val SEC_CALL = Value("second call")
    val TER_CALL = Value("third call")
    val WAIT_LOCS_CLICK_FIRST = Value("wait for first click")
    val WAIT_LOCS_CLICK_SECOND = Value("wait for second click")
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

object CommandTriggerType extends Enumeration {
    type CommandTriggerType = Value
    val KEY   = Value("key pressed (outside cmdline)")
    val CMD   = Value("command entered in cmdline)")
    val CLICK = Value ("Mouse click")
}
import CommandTriggerType._

/*
*  The Command class defines several CommandManager inherited classes
*  | these handle a certain type of commands
*  | and keep separate history, (un|re)do lists, ...
*  The Command class interacts with the BodyPart.
*
*  A Room object is required to initiate a Command or Command Management
*  class to access logs / cmdline / items / organisms(.) / ...
*/



abstract class CommandManager (val room: Room, val acceptedCommands: List[String]) {
    /*
    ** Appends a string to the log TextArea of room.body
    */
    def appendLogs (str: String): Unit { room.body.logs.text += str  }



    /*
    ** useful variables
    */
    var undo: List[() => Unit] = List()
    var redo: List[() => Unit] = List()     //
    var history: List[String] = List()      // History of all entered commands (composed commands are tight)
    var help_menus = List[String] = List()  // List of the help entries concerning the command handler



    /*
    ** Definitions of the Command-interception methods
    */
    def acceptCommand (str: String, type: CommandTriggerType): Boolean = {      // True if the command is handled (rq: multi-line def => com' alignment looks better)
        acceptedCommands.exists(x => x == str.split(" ").head)
    }
    def executeCommand (str: String, type: CommandTriggerType): Boolean         // True if the execution is over (false if waiting for interaction)
    def executeIfAcceptCommand (str: String, type: CommandTriggerType): Boolean // True if the command is over
}



class DirectionsCommand extends CommandManager (val room: Room, val acceptedCommands: List[String]) {
}



class SelectionCommand extends CommandManager (val room: Room) {
}



class OrganismsCommand extends CommandManager (val room: Room) {
}



class ItemsCommand extends CommandManager (val room: Room) {
}





class Command (val body: BodyPart, val room: Room, val player: Player) {
}
