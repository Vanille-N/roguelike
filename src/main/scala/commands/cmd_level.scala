// This class is responsible for
// - level loading (go back to a previous level)
// - save/load
class LevelCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("level", "load", "save")
    help_menus = "level" :: Nil

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        publish(HeyPrint(prompt + unSplitCommand(splited_command_arg)))
        var splited_command: Array[String] = splited_command_arg // mutable copy
        def level_load: String = {
            if (!command_syntax_check(
                splited_command,
                Array(
                    (true, "level"),
                    (true, s"N:1->${room.body.level.max}")
                )
            )) {
                publish(HeyPrint("The command does not fit its syntax.\n\tCheck `help level`."))
                return ""
            }

            splited_command.length match {
                case 1 => {
                    publish(HeyPrint(s"Enter level you want to go back to.\n\tCurrent: ${room.body.level.num}\n\tUnlocked : 1 to ${room.body.level.max}"))
                    return "level"
                }
                case 2 => {
                    publish(LevelLoad(splited_command(1).toInt))
                    return ""
                }
                case _ => { publish(HeyPrint("Illegal number of arguments")); return "" }
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
                publish(HeyPrint("The command does not fit its syntax.\n\tCheck `help level`."))
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
                    publish(HeyPrint(msg))
                    return "load"
                }
                case 2 => {
                    if (splited_command(1) == "l") {
                        publish(SaveList())
                        return ""
                    }
                    publish(GameLoad(splited_command(1)))
                    return ""
                }
                case _ => { publish(HeyPrint("Illegal number of arguments")); return "" }
            }
        }

        def game_save: String = {
            splited_command.length match {
                case 1 => {
                    publish(HeyPrint("Enter name of save file to write to"))
                    return "save"
                }
                case 2 => {
                    publish(GameSave(splited_command(1)))
                    return ""
                }
                case _ => { publish(HeyPrint("Illegal number of arguments")); return "" }
            }
        }


        splited_command(0) match {
            case "level" => { return level_load }
            case "save" => { return game_save }
            case "load" => { return game_load }
            case _ => { publish(HeyPrint(s"Error: Command `${splited_command(0)}` unknown")) }
        }
        return ""
    }
}

