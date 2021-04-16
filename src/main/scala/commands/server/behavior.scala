import Behavior._
class ServerBehaviorCommand (room: Room) extends ServerCommandManager (room) {

	def execute (splited_command: Array[String]): Boolean = {
		publish(PrintInLogs(prompt + unSplitCommand(splited_command)))
	def behavior: Boolean = {
			val newCmd = splited_command(1) match {
				case "cursor" | "1" => "behavior-cursor"
				case "target" | "2" | "click" => "behavior-target"
				case "give" | "3" => "behavior-give"
				case "keep" | "4" => "behavior-keep"
				case _ => { publish(PrintInLogs("Error: no such behavior")); "" }
			}
			var newSplit = splited_command.tail
			newSplit(0) = newCmd
			return execute(newSplit)
		}

		def behavior_cursor: Boolean = {
			room.body.selection_organisms(room.body.selection_names.indexOf(room.body.selection_current))._1.foreach(o => {
				o.behavior = { () => (room.body.player.position, SEEK) }
			})
			publish(PrintInLogs("The friendly organisms have changed their target"))
			return true
		}

		def behavior_give: Boolean = {
			room.body.player.itemPolicyTake = true
			true
		}

		def behavior_keep: Boolean = {
			room.body.player.itemPolicyTake = false
			true
		}

		def behavior_target: Boolean = {
			val i = splited_command(1).toInt
			val j = splited_command(2).toInt
			room.body.selection_organisms(room.body.selection_names.indexOf(room.body.selection_current))._1.foreach(o => {
				o.behavior = { () => (room.locs(i, j), SEEK) }
				if(!o.isFriendly) publish(PrintInLogs("dskjfh kdsjhf klf flkqs"))
			})
			publish(PrintInLogs("The friendly organisms have changed their target"))
			return true
		}

		splited_command(0) match {
			case "behavior" => { return behavior }
			case "behavior-cursor" => { return behavior_cursor }
			case "behavior-target" => { return behavior_target }
			case "behavior-keep" => { return behavior_keep }
			case "behavior-give" => { return behavior_give }
			case _ => { publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown")) }
		}
		return true
	}
}
