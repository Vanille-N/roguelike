import java.util.concurrent.TimeUnit
import akka.actor._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import event._

import Direction._


// The following  class Serverdeals with the movements of the player
class ServerDirectionsCommand(room: Room) extends ServerCommandManager (room) {
	def execute (splited_command: Array[String]): Boolean = {
		splited_command(0) match {
			case "Up"	=> { room.body.player.move(UP)	}
			case "Down"  => { room.body.player.move(DOWN)  }
			case "Left"  => { room.body.player.move(LEFT)  }
			case "Right" => { room.body.player.move(RIGHT) }
			case _	   => { publish(PrintInLogs(s"Error: Direction `${splited_command(0)}` unknown")) }
		}
		room.locs.map(_.updateVisuals)// Update the map that the user sees.
		return true
	}
}



// The following class Serverdeals withe the repeat variable (allowing to advance by `repeat` steps the state of the game).
class ServerDigitsCommand(room: Room) extends ServerCommandManager (room) {
	def execute (splited_command: Array[String]): Boolean = {
		splited_command(0) match {
			case "0"			=> { room.body.repeat *= 10; return true }
			case "1"			=> { room.body.repeat = room.body.repeat * 10 + 1; return false }
			case "2"			=> { room.body.repeat = room.body.repeat * 10 + 2; return false }
			case "3"			=> { room.body.repeat = room.body.repeat * 10 + 3; return false }
			case "4"			=> { room.body.repeat = room.body.repeat * 10 + 4; return false }
			case "5"			=> { room.body.repeat = room.body.repeat * 10 + 5; return false }
			case "6"			=> { room.body.repeat = room.body.repeat * 10 + 6; return false }
			case "7"			=> { room.body.repeat = room.body.repeat * 10 + 7; return false }
			case "8"			=> { room.body.repeat = room.body.repeat * 10 + 8; return false }
			case "9"			=> { room.body.repeat = room.body.repeat * 10 + 9; return false }
			case "+"			=> { room.body.repeat += 1; return false }
			case "-"			=> { room.body.repeat -= 1; return false }
			case "repeat-reset" => { room.body.repeat = 1; return false }
			case "repeat"	   => { if (splited_command.length == 1) room.body.repeat = 1 else room.body.repeat = splited_command(1).toInt; return false }
			case _	   => { publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown")) }
		}
		true
	}
}



class ServerOtherCommand (room: Room) extends ServerCommandManager (room) {
	// The two following variables are used to make an automatic and regular step in the game evolution.
	def scheduler: Scheduler = ActorSystem.create("timer-example").scheduler
	var runner: Cancellable = null

	def execute (splited_command: Array[String]): Boolean = {
		def other_play: Boolean = {// start the automatic step by step
			if (room.body.isPlaying) return false
			room.body.isPlaying = true
			if (splited_command.length == 1) {
				runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(1,TimeUnit.SECONDS)) { room.body.step }
			} else {
				runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(splited_command(1).toInt,TimeUnit.SECONDS)) { room.body.step }
			}
			return false
		}
		def other_stop : Boolean = {// stop the automatic step by step.
			if(room.body.isPlaying) { runner.cancel(); room.body.isPlaying = false; true }
			else return false
		}
		def other_toggle : Boolean = {// toggle the step by step.
			if(room.body.isPlaying) { other_stop }
			else { other_play }
		}
		def other_step: Boolean = {// make a/n step
			if(splited_command.length == 1) { room.body.step }
			else { for(i <- 1 to (splited_command(1).toInt)) room.body.step; }
			return false
		}

		splited_command(0) match {// main switch to define which function fits with the command at hand.
			case "play"		  => { return other_play }
			case "stop"		  => { return other_stop }
			case "toggle"		=> { return other_toggle }
			case "quit"		  => { execute(Array[String]("stop")) ; Runtime.getRuntime().halt(0) }
			case "step"		  => { return other_step }
			case "step-multiple" => { execute(Array[String]("step", room.body.repeat.toString)); room.body.repeat = 1; return false }
			case "focus-cmdline" => { room.body.cmdline.requestFocusInWindow(); return false }
			case "focus-win"	 => { room.body.globalPanel.requestFocusInWindow(); return false }
			case "q"			 => { room.body.globalPanel.requestFocusInWindow(); return false }
			case "clear"		 => { publish(ClearLogs()); return false }
			case "sacrifice"	 => { publish(Sacrifice()); return false }
			case _			   => { publish(PrintInLogs("Error: Command `" + splited_command(0) + "` unknown")); return false }
		}
		return false
	}
}



// accept every command and do nothing.
class ServerNullCommand (room: Room) extends ServerCommandManager (room) {
	val acceptedCommands: List[String] = List()

	def execute (splited_command: Array[String]): Boolean = {
		publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown"))
		return false
	}
}
