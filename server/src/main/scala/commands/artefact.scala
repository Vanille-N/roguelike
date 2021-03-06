import collection.mutable.Map

import ArtefactType._

// The following class deals with the artefacts management
class ArtefactsCommand (body: BodyPart, game: Game)
extends ClientCommandManager (body, game) {
	// Definition of the first words of a command that are acceptes as artefact
	// commands and help commands that may be usefull
    val acceptedCommands: List[String] = List("artefact", "artefact-add", "artefact-rm", "artefact-list")
    help_menus = "artefact" :: Nil

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        publish(PrintInLogs(prompt + unSplitCommand(splited_command_arg)))
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
			/* This function is used as a redirection hub for artefacts commands
			** it makes it simplier to call such commands: `artefact add` is
			** more natural than `artefact-add`
			*/
            splited_command.length match {
                case 1 => { publish(PrintInLogs("Wrong usage of command `artefact`\n\t-> check `help artefact` to find out :)")); return "" }
                case _ => {
                    splited_command(1) match {
                        case "list" => {artefacts_list; return ""}
                        case "rm" => {
                            splited_command = splited_command.tail
                            splited_command(0) = "artefact-rm"
                            return artefacts_rm
                        }
                        case "add" => {
                            splited_command = splited_command.tail
                            splited_command(0) = "artefact-add"
                            return artefacts_add
                        }
                    }
                }
            }
        }

        def artefacts_add: String = {
			// First, check the syntax of the command entered in the command
			// line
            if(!command_syntax_check (
                splited_command,
                Array(
                    (true, "artefact-add"),
                    (true, s"l|N:0->${available_classes.size - 1};"),
                    (true, s"l|N:0->${available_types.size - 1};"),
                    (true, "N:0->5;"),
                    (false, s"N:1->${body.room.rows};"),
                    (true,  s"N:1->${body.room.cols};")
                    )
                )) {
                publish(PrintInLogs("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }
			// The command line respects the command syntax: check the command
			// length means check the interaction state
            splited_command.length match {
                case 1 => {// command is: 'artefact-add'
                    publish(PrintInLogs(s"Which type of artefact would you like to add ?(l to list)"))
                    return unSplitCommand(splited_command)
                }
                case 2 => {// command is: 'artefact-add <l | type>'
                    splited_command(1) match {
                        case "l" => {
                            var liste: String = ""
                            for (i <- 0 to (available_classes.size-1)) liste += s"\n\t$i -> ${available_classes(i)}"
                            publish(PrintInLogs(s"$liste\n\nWhich type of artefact would you like to add ?(l to list)"))
                            return "artefact-add"
                        }
                        case _ => {
                            publish(PrintInLogs("What should the artefact do ? (l to list)"))
                            return unSplitCommand(splited_command)
                        }
                    }
                }
                case 3 => {// command is 'artefact-add <type> <l | sub-type>'
                    splited_command(2) match {
                        case "l" => {
                            var liste: String = ""
                            for (i <- 0 to available_types.size - 1) {
                                liste += s"\n\t$i -> ${available_types(i)}"
                            }
                            publish(PrintInLogs(s"${liste}\n\nWhat should the artefact do ? (l to list)"))
                            return s"artefact-add ${splited_command(1)}"
                        }
                        case _ => {
                            publish(PrintInLogs("What level should the artefact have ? (1 -> 5)"))
                            return unSplitCommand(splited_command)
                        }
                    }
                }
					case 4 => {// command is: 'artefact-add <type> <sub-type> <level>'
                    publish(PrintInLogs("Where do you want to spawn the artefact ?(`i j ` or click"))
                    return unSplitCommand(splited_command)
                }
                case 6 => {// command is: 'artefact-add <type> <sub-type> <level> <coord_i> <coord_j>'
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
                    body.room.locs(i, j).artefacts += {splited_command(1).toInt match {
                        case 0 => new Artefact(body.room.locs(i, j), 5, target_level, artefact_type)
                        case 1 => new Murderer(body.room.locs(i, j), 5, target_level, artefact_type)
                        case 2 => new ForceUsage(body.room.locs(i, j), 5, target_level, artefact_type)
                        case 3 => new Temptation(body.room.locs(i, j), 5, target_level, artefact_type)
                        case 4 => new Unattach(body.room.locs(i, j), 5, target_level, artefact_type)
                    }}
                    return ""
                }
                case _ => return ""
            }
        }

        def artefacts_rm: String = {
			// This function destroys an artefact. It begins by loading the artefacts and their indexes
            val artefacts: Map[Int, Artefact] = Map()
            var i: Int = 0
            body.room.locs.map(_.artefacts.foreach(
                a => {
                    artefacts += (i -> a)
                    i += 1
                    }
                ))
			// Secondly, check the command syntax
            if(!command_syntax_check(
                splited_command,
                Array (
                    (true, "artefact-rm"),
                    (true, s"l|N:0->${artefacts.size - 1};")
                    )
                )) {
                publish(PrintInLogs("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }
			// Then, check the interactivity status
            splited_command.length match {
                case 1 => {// command is: 'artefact-rm'
                        publish(PrintInLogs("Which artefact would you like to destroy ?(l to list)"))
                        return "artefact-rm"
				}
                case 2 => {// command is: 'artefact-rm <l | artefact_index>'
                    if(splited_command(1) == "l") {
                        artefacts_list
                        publish(PrintInLogs("Which artefact would you like to destroy ?(l to list)"))
                        return "artefact-rm"
                    } else {
                        artefacts(splited_command(1).toInt).position.artefacts -= artefacts(splited_command(1).toInt)
                        publish(PrintInLogs(s"Artefact ${artefacts(splited_command(1).toInt).toString} destroyed"))
                    }
                }
            }
            ""
        }

        def artefacts_list: Unit = {
            val artefacts: Map[Int, String] = Map()
            var i: Int = 0
            body.room.locs.map(_.artefacts.foreach(
                a => {
                    artefacts += (i -> a.toString)
                    i += 1
                    }
                ))
            publish(PrintInLogs(s"   ---------   Printing ${artefacts.size} elements   ---------"))
            for (i <- 0 to artefacts.size - 1) publish(PrintInLogs(s"\n\t$i -> ${artefacts(i)}"))
            publish(PrintInLogs(s"   ---------   End of the printing of ${artefacts.size} elements   ---------"))
        }

		// The following match decides which function is to use for the given command.
        splited_command(0) match {// main switch to defines the function which corresponds to the command at hand.
            case "artefact"      => { return artefacts_artefact }
            case "artefact-add"  => { return artefacts_add }
            case "artefact-rm"   => { return artefacts_rm }
            case "artefact-list" => { artefacts_list; return "" }
            case _               => { publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown")); return "" }
        }
        return ""
    }
}
