import Behavior._

class BehaviorCommand (body: BodyPart, game: Game)
extends ClientCommandManager (body, game) {
	// Definition of the first words of a command that are acceptes as artefact
	// commands and help commands that may be usefull
    val acceptedCommands: List[String] = List("behavior", "behavior-cursor", "behavior-target", "behavior-give", "behavior-keep")
    help_menus = "behavior" :: Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        publish(PrintInLogs(prompt + unSplitCommand(splited_command)))
        def behavior: String = {
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                        (true, "behavior"),
                        (true, "target|cursor|click|give|keep|N:1->4;")
                    )
                )) {
                publish(PrintInLogs("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }

            // The syntax is correct. Check the interaction state.
            splited_command.length match {
                case 1 => {// command is: 'behavior'
                    publish(PrintInLogs("To control movement: 1 -> cursor, 2 -> click target\nTo control items: 3 -> give, 4 -> keep"))
                    return "behavior"
                }
                case 2 => {// command is 'behavior <cursor | 1 | target | 2 | give | 3 | keep | 4 >'
                    val newCmd = splited_command(1) match {
                        case "cursor" | "1" => "behavior-cursor"
                        case "target" | "2" | "click" => "behavior-target"
                        case "give" | "3" => "behavior-give"
                        case "keep" | "4" => "behavior-keep"
                        case _ => { publish(PrintInLogs("Error: no such behavior")); "" }
                    }
                    var newSplit = splited_command.tail
                    newSplit(0) = newCmd
                    return realExecuteCommand(newSplit)
                }
                case _ => {
                    publish(PrintInLogs("Error: too many parameters; see `help behavior`\nAborting."))
                    return ""
                }
            }
        }

        def behavior_cursor: String = {
			// This function is executed if the selected cells are to follow the cursor
            game.selection_organisms(game.selection_names.indexOf(game.selection_current)).foreach(o => {
                o.behavior = { () => (game.player.position, SEEK) }
            })
            publish(PrintInLogs("The friendly organisms have changed their target"))
            return ""
        }

        def behavior_give: String = {
            game.player.itemPolicyTake = true
            ""
        }

        def behavior_keep: String = {
            game.player.itemPolicyTake = false
            ""
        }

        def behavior_target: String = {
			// This function is executed if the selected cells must get to a certain location

            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                        (true,  "behavior-target"),
                        (false, s"N:1->${body.room.rows - 1};"),
                        (true,  s"N:1->${body.room.cols - 1};")
                    )
                )) {
                publish(PrintInLogs("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }

            // The syntax is correct. Check the interaction state.
            splited_command.length match {
                case 1 => {// command is: 'behavior-target'
                    publish(PrintInLogs("Which tile ?"))
                    return "behavior-target"
                }
                case 3 => {// command is: 'behavior-target <coord_i> <coord_j>'
                    val i = splited_command(1).toInt
                    val j = splited_command(2).toInt
                    game.selection_organisms(game.selection_names.indexOf(game.selection_current)).foreach(o => {
                        o.behavior = { () => (body.room.locs(i, j), SEEK) }
                    })
                    publish(PrintInLogs("The friendly organisms have changed their target"))
                    return ""
                }
                case _ => { publish(PrintInLogs("Illegal number of arguments. Aborting.")) }
            }
            return ""
        }
        println(splited_command(0))

		// The following match decides which function is to use for the given command.
        splited_command(0) match {
            case "behavior" => { return behavior }
            case "behavior-cursor" => { return behavior_cursor }
            case "behavior-target" => { return behavior_target }
            case "behavior-keep" => { return behavior_keep }
            case "behavior-give" => { return behavior_give }
            case _ => { publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown")) }
        }
        return ""
    }
}
