import concurrent.duration.FiniteDuration
import concurrent.ExecutionContext.Implicits.global

import java.util.concurrent.TimeUnit
import akka.actor._

import Direction._


// The following  class deals with the movements of the player
class DirectionsCommand(body: BodyPart, game: Game)
extends ClientCommandManager (body, game) {
    val acceptedCommands: List[String] = List("Up", "Down", "Left", "Right")
    help_menus = "directions" :: Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        splited_command(0) match {
            case "Up"    => { game.player.move(UP)    }
            case "Down"  => { game.player.move(DOWN)  }
            case "Left"  => { game.player.move(LEFT)  }
            case "Right" => { game.player.move(RIGHT) }
            case _       => { publish(PrintInLogs(s"Error: Direction `${splited_command(0)}` unknown")) }
        }
        ""
    }
}



// The following class deals withe the repeat variable (allowing to advance by `repeat` steps the state of the game).
class DigitsCommand(body: BodyPart, game: Game)
extends ClientCommandManager (body, game) {
    val acceptedCommands: List[String] = List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "+", "-", "repeat-reset")
    help_menus = "repeat" :: Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        splited_command(0) match {
            case "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => {
                body.repeat = body.repeat * 10 + splited_command(0).toInt;
                return ""
            }
            case "+"            => { body.repeat += 1; return "" }
            case "-"            => { body.repeat -= 1; return "" }
            case "repeat-reset" => { body.repeat = 1; return "" }
            case "repeat"       => { if (splited_command.length == 1) body.repeat = 1 else body.repeat = splited_command(1).toInt; return "" }
            case _       => { publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown")) }
        }
        return ""
    }
}



class OtherCommand (body: BodyPart, game: Game)
extends ClientCommandManager (body, game) {
    val acceptedCommands: List[String] = List("contents", "clear", "sacrifice")
    help_menus = Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        def showContents {
            println(splited_command, splited_command(1), splited_command(2))
            if (!command_syntax_check(
                splited_command,
                Array(
                    (false, "contents"),
                    (false, s"N:1->${body.room.rows - 1};"),
                    (true, s"N:1->${body.room.cols - 1};")
                )
            )) {
                publish(PrintInLogs("Syntax: `contents $i $j`"))
                return
            }
            val i = splited_command(1).toInt
            val j = splited_command(2).toInt
            publish(PrintInLogs(body.room.locs(i,j).listContents))
        }
        
        splited_command(0) match {// main switch to define which function fits with the command at hand.
            case "clear"         => { publish(ClearLogs()); "" }
            case "sacrifice"     => { publish(Sacrifice()); "" }
            case "contents"      => { showContents; "" }
            case _               => { publish(PrintInLogs("Error: Command `" + splited_command(0) + "` unknown")); "" }
        }
    }
}



// accept every command and do nothing.
class NullCommand (body: BodyPart, game: Game)
extends ClientCommandManager (body, game) {
    val acceptedCommands: List[String] = List()
    override def acceptCommand (str: String): Boolean = { true }
    help_menus = Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown"))
        return ""
    }
}
