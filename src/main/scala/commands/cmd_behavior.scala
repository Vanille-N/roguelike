class BehaviorCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("behavior", "behavior-cursor", "behavior-target")
    help_menus = "behavior" :: Nil
    import Behavior._

    def realExecuteCommand (splited_command: Array[String]): String = {
        appendLogs(prompt + unSplitCommand(splited_command))
        def behavior: String = {
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                        (true, "behavior"),
                        (true, "target|cursor|click|N:1->2;")
                    )
                )) {
                appendLogs("The command does not fit its syntax :/\n\tAborting.")
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    appendLogs("Where should they go ? (1 -> cursor, 2 -> click target)")
                    return "behavior"
                }
                case 2 => {
                    return splited_command(1) match {
                        case "cursor" | "1" => "behavior-cursor"
                        case "target" | "2" | "click" => "behavior-target"
                        case _ => { appendLogs("Error: no such behavior"); "" }
                    }
                }
                case _ => {
                    appendLogs("Error: too many parameters; see `help behavior`\nAborting.")
                    return ""
                }
            }
        }

        def behavior_cursor: String = {
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                        (true, "behavior-cursor")
                    )
                )) {
                appendLogs("The command does not fit its syntax :/\n\tAborting.")
                return ""
            }

            // The syntax is correct. Continue.
            room.body.organisms_selection.foreach(o => {
                if (o.isFriendly) {
                    o.behavior = { () => (room.body.player.position, SEEK) }
                }
            })
            appendLogs("The friendly organisms have changed their target")
            return ""
        }

        def behavior_target: String = {
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                        (true,  "behavior-target"),
                        (false, s"N:1->${room.rows - 1};"),
                        (true,  s"N:1->${room.cols - 1};")
                    )
                )) {
                appendLogs("The command does not fit its syntax :/\n\tAborting.")
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    appendLogs("Which tile ?")
                    return "behavior-target"
                }
                case 2 => {
                    appendLogs ("Illegal number of parameters (plausible error: coordinates must be passed as `i j`, not `i`, `j`.)\n\tAborting.")
                    return ""
                }
                case 3 => {
                    val i = splited_command(1).toInt
                    val j = splited_command(2).toInt
                    room.body.organisms_selection.foreach(o => {
                        if (o.isFriendly) {
                            o.behavior = { () => (room.locs(i, j), SEEK) }
                        }
                    })
                    appendLogs("The friendly organisms have changed their target")
                    return ""
                }
                case _ => { appendLogs("Illegal number of arguments. Aborting.") }
            }
            return ""
        }

        splited_command(0) match {
            case "behavior" => { return behavior }
            case "behavior-cursor" => { return behavior_cursor }
            case "behavior-target" => { return behavior_target }
            case _ => { appendLogs(s"Error: Command `${splited_command(0)}` unknown") }
        }
        return ""
    }
}
