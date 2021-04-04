import java.lang.NumberFormatException

// This class is responsible for
// - level loading (go back to a previous level)
// - save/load
class LevelCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("level", "load", "save")
    help_menus = "level" :: Nil

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        publish(PrintInLogs(prompt + unSplitCommand(splited_command_arg)))
        var splited_command: Array[String] = splited_command_arg // mutable copy
        def level_load: String = {
            if (!command_syntax_check(
                splited_command,
                Array(
                    (true, "level"),
                    (true, s"N:1->${room.body.level.max};")
                )
            )) {
                publish(PrintInLogs("The command does not fit its syntax.\n\tCheck `help level`."))
                return ""
            }

            splited_command.length match {
                case 1 => {
                    publish(PrintInLogs(s"Enter level you want to go back to.\n\tCurrent: ${room.body.level.num}\n\tUnlocked : 1 to ${room.body.level.max}"))
                    return "level"
                }
                case 2 => {
                    publish(LevelLoad(splited_command(1).toInt))
                    return ""
                }
                case _ => { publish(PrintInLogs("Illegal number of arguments")); return "" }
            }
        }

        def game_load: String = {
            if (!command_syntax_check(
                splited_command,
                Array(
                    (true, "load"),
                    (true, "[abcdefghijklmnopqrstuvwxyz_]|N"),
                )
            )) {
                publish(PrintInLogs("The command does not fit its syntax.\n\tCheck `help level`."))
                return ""
            }

            splited_command.length match {
                case 1 => {
                    val saves = GameLoader.listSaveFiles
                    var msg = ""
                    for ((f, i) <- saves.zipWithIndex) {
                        msg += s"  $i: $f\n"
                    }
                    msg += "Enter name or number of save file to load"
                    publish(PrintInLogs(msg))
                    return "load"
                }
                case 2 => {
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
                    return ""
                }
                case _ => { publish(PrintInLogs("Illegal number of arguments")); return "" }
            }
        }

        def game_save: String = {
            splited_command.length match {
                case 1 => {
                    publish(PrintInLogs("Enter name of save file to write to"))
                    return "save"
                }
                case 2 => {
                    publish(GameSave(splited_command(1)))
                    return ""
                }
                case _ => { publish(PrintInLogs("Illegal number of arguments")); return "" }
            }
        }


        splited_command(0) match {
            case "level" => { return level_load }
            case "save" => { return game_save }
            case "load" => { return game_load }
            case _ => { publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown")) }
        }
        return ""
    }
}

