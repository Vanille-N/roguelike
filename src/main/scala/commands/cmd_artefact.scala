import ArtefactType._

// The following class deals with the artefacts management
class ArtefactsCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("artefact", "artefact-add", "artefact-rm", "artefact-list")
    help_menus = "artefact" :: Nil

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        publish(HeyPrint(prompt + unSplitCommand(splited_command_arg)))
        var splited_command: Array[String] = splited_command_arg// necessary as the arguments of a function are non mutable

        val available_classes: Map[Int, String] = Map (
            (0, "Casual artefact"),
            (1, "Murderer"),
            (2, "ForceUsage"),
            (3, "Tentation"),
            (4, "Unattach")
        )
        val available_types: Map[Int, String] = Map (
            (0, "level up"),
            (1, "level down"),
            (2, "level set"),
            (3, "level double"),
            (4, "level divide")
        )

        def artefacts_artefact: String = {
            ""
        }

        def artefacts_add: String = {
            splited_command.length match {
                case 1 => {
                    publish(HeyPrint(s"Which type of artefact would you like to add ?(l to list)"))
                    return unSplitCommand(splited_command)
                }
                case 2 => {
                    splited_command(1) match {
                        case "l" => {
                            var liste: String = ""
                            for (i <- 0 to available_classes.size) liste += s"\n\t$i -> ${available_classes(i)}"
                            publish(HeyPrint(s"$liste\n\nWhich type of artefact would you like to add ?(l to list)"))
                            return "artefact-add"
                        }
                        case _ => {
                            publish(HeyPrint("What should the artefact do ? (l to list)"))
                            return "artefact-add ${splited_command(1)}"
                        }
                    }
                }
                case 3 => {
                    splited_command(2) match {
                        case "l" => {
                            var liste: String = ""
                            for (i <- 0 to available_types.size) {
                                liste += available_types(i)
                            }
                            publish(HeyPrint(s"${liste}\n\nWhat should the artefact do ? (l to list)"))
                            return s"artefact-add ${splited_command(1)}"
                        }
                        case _ => {
                            publish(HeyPrint("What level should the artefact have ? (1 -> 5)"))
                            return unSplitCommand(splited_command)
                        }
                    }
                }
                case 4 => {
                    publish(HeyPrint("Where do you want to spawn the artefact ?(`i j ` or click"))
                    return unSplitCommand(splited_command)
                }
                case 6 => {
                    val i: Int = splited_command(4).toInt
                    val j: Int = splited_command(5).toInt
                    val target_level: Int = splited_command(3).toInt
                    val artefact_type: ArtefactType = splited_command(2).toInt match {
                        case 0 => LEVELUP
                        case 1 => LEVELDOWN
                        case 2 => LEVELSET
                        case 3 => LEVELDOUBLE
                        case 4 => LEVELDDOUBLE
                    }
                    room.locs(i, j).artefacts += {splited_command(1).toInt match {
                        case 0 => new Artefact(room.locs(i, j), 5, target_level, artefact_type)
                        case 1 => new Murderer(room.locs(i, j), 5, target_level, artefact_type)
                        case 2 => new ForceUsage(room.locs(i, j), 5, target_level, artefact_type)
                        case 3 => new Temptation(room.locs(i, j), 5, target_level, artefact_type)
                        case 4 => new Unattach(room.locs(i, j), 5, target_level, artefact_type)
                    }}
                    return ""
                }
                case _ => return ""
            }
        }

        def artefacts_rm: String = {
            ""
        }

        def artefacts_list: String = {
            ""
        }

        splited_command(0) match {// main switch to defines the function which corresponds to the command at hand.
            case "artefact"      => { return artefacts_artefact }
            case "artefact-add"  => { return artefacts_add }
            case "artefact-rm"   => { return artefacts_rm }
            case "artefact-list" => { return artefacts_list }
            case _               => { publish(HeyPrint(s"Error: Command `${splited_command(0)}` unknown")); return "" }
        }
        return ""
    }
}
