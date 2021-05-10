import java.util.concurrent.TimeUnit
import akka.actor._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import event._

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
        publish(RefreshDisplay())
        return ""
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
    val acceptedCommands: List[String] = List("quit", "toggle", "stop", "play", "step", "step-multiple", "focus-cmdline", "focus-win", "q", "clear", "sacrifice")
    help_menus = Nil

    // The two following variables are used to make an automatic and regular step in the game evolution.
    def scheduler: Scheduler = ActorSystem.create("timer-example").scheduler
    var runner: Cancellable = null

    def realExecuteCommand (splited_command: Array[String]): String = {
        def other_play: String = {// start the automatic step by step
            if (body.isPlaying) return ""
            body.isPlaying = true
            if (splited_command.length == 1) {
                runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(1,TimeUnit.SECONDS)) { body.step }
            } else {
                runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(splited_command(1).toInt,TimeUnit.SECONDS)) { body.step }
            }
            return ""
        }
        def other_stop : String = {// stop the automatic step by step.
            if (body.isPlaying) { runner.cancel(); body.isPlaying = false; "" }
            else return ""
        }
        def other_toggle : String = {// toggle the step by step.
            if (body.isPlaying) { other_stop }
            else { other_play }
        }
        def other_step: String = {// make a/n step
            if (splited_command.length == 1) { body.step }
            else { for(i <- 1 to (splited_command(1).toInt)) { body.step } }
            return ""
        }

        splited_command(0) match {// main switch to define which function fits with the command at hand.
            case "play"          => { return other_play }
            case "stop"          => { return other_stop }
            case "toggle"        => { return other_toggle }
            case "quit"          => { realExecuteCommand(Array[String]("stop")) ; Runtime.getRuntime().halt(0) }
            case "step"          => { return other_step }
            case "step-multiple" => { realExecuteCommand(Array[String]("step", body.repeat.toString)); body.repeat = 1; return "" }
            case "clear"         => { publish(ClearLogs()); return "" }
            case "sacrifice"     => { publish(Sacrifice()); return "" }
            case _               => { publish(PrintInLogs("Error: Command `" + splited_command(0) + "` unknown")); return "" }
        }
        return ""
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
