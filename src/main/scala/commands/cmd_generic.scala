import java.util.concurrent.TimeUnit
import akka.actor._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import event._

import Direction._



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
