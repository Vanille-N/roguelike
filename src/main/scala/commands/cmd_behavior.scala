class BehaviorCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("behavior", "behavior-cursor", "behavior-target", "behavior-give", "behavior-keep")
    help_menus = "behavior" :: Nil
    import Behavior._

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

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    publish(PrintInLogs("To control movement: 1 -> cursor, 2 -> click target\nTo control items: 3 -> give, 4 -> keep"))
                    return "behavior"
                }
                case 2 => {
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
            room.body.selection_organisms(room.body.selection_names.indexOf(room.body.selection_current))._1.foreach(o => {
                o.behavior = { () => (room.body.player.position, SEEK) }
            })
            publish(PrintInLogs("The friendly organisms have changed their target"))
            return ""
        }

        def behavior_give: String = {
            room.body.player.itemPolicyTake = true
            ""
        }

        def behavior_keep: String = {
            room.body.player.itemPolicyTake = false
            ""
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
                publish(PrintInLogs("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    publish(PrintInLogs("Which tile ?"))
                    return "behavior-target"
                }
                case 2 => {
                    publish(PrintInLogs("Illegal number of parameters (plausible error: coordinates must be passed as `i j`, not `i`, `j`.)\n\tAborting."))
                    return ""
                }
                case 3 => {
                    val i = splited_command(1).toInt
                    val j = splited_command(2).toInt
                    room.body.selection_organisms(room.body.selection_names.indexOf(room.body.selection_current))._1.foreach(o => {
                        o.behavior = { () => (room.locs(i, j), SEEK) }
                        if(!o.isFriendly) publish(PrintInLogs("dskjfh kdsjhf klf flkqs"))
                    })
                    publish(PrintInLogs("The friendly organisms have changed their target"))
                    return ""
                }
                case _ => { publish(PrintInLogs("Illegal number of arguments. Aborting.")) }
            }
            return ""
        }
        println(splited_command(0))

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
