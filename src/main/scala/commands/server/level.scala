import java.lang.NumberFormatException

// This class Serveris responsible for
// - level loading (go back to a previous level)
// - save/load
class ServerLevelCommand (room: Room) extends ServerCommandManager (room) {
	def execute (splited_command_arg: Array[String]): Boolean = {
		publish(PrintInLogs(prompt + unSplitCommand(splited_command_arg)))
		var splited_command: Array[String] = splited_command_arg // mutable copy
		def level_load: Boolean = {
			publish(LevelLoad(splited_command(1).toInt))
			true
		}

		def game_load: Boolean = {
			try {
				val index = splited_command(1).toInt
				val saves = GameLoader.listSaveFiles
				if (index < saves.length) {
					publish(PrintInLogs("Loading level"))
					publish(GameLoad(GameLoader.loadFile(saves(index))))
				} else {
					publish(PrintInLogs("No such file"))
				}
			} catch {
				case _: NumberFormatException => {
					val game = GameLoader.tryLoadFile(splited_command(1))
					if (game != null) {
						publish(PrintInLogs("Loading level"))
						publish(GameLoad(game))
					} else {
						publish(PrintInLogs("No such file"))
					}
				}
			}
			true
		}

		def game_save: Boolean = {
			publish(GameSave(splited_command(1)))
			return true
		}


		splited_command(0) match {
			case "level" => { return level_load }
			case "save" => { return game_save }
			case "load" => { return game_load }
			case _ => { publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown")) }
		}
		return true
	}
}

