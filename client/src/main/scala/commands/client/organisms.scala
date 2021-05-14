// The following class deals with the management of the organisms
class OrganismsCommand (body: BodyPart, game: Game)
extends ClientCommandManager (body, game) {
    val acceptedCommands: List[String] = List("list", "set", "show")
    var last_checked_arg: Int = 0
    help_menus = "list" :: "set" :: "show" :: Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        publish(PrintInLogs(prompt + unSplitCommand(splited_command)))
        def getOrganismById (id: Int): Organism = {
            val lily : List[Organism] = body.organisms.toList
            if(id > lily.length) { null }
            else {
                var i: Int = 0
                for ( o <- lily ) {
                    if (i == id) {return o}
                    i += 1
                }
            }
            null
        }

        def organisms_list: Unit = {
            publish(PrintInLogs("   ---   Printing " + body.organisms.size + " organisms:   ---"))
            var i: Int = 0
            for ( o <- body.organisms.toList ) {
                publish(PrintInLogs(i + "-\t" + o))
                i += 1
            }
            publish(PrintInLogs("   ---   End of the list ( " + body.organisms.size +"organisms).   ---", ln_before = true))
        }

        def organisms_set: String = {// Allows the user to set a stat field of any organism to any integer value.
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                        (true, "set"),
                        (true, "l|N:0->" + (body.organisms.size - 1) + ";"),
                        (true, "SPD|HP|POW|DEF|DEC"),
                        (true, "N")
                    )
                )) {
                publish(PrintInLogs("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    publish(PrintInLogs("Which organism would you like to consider? (l to list available organisms)"))
                    return "set"
                }
                case 2 => {
                    if(splited_command(1) == "l") {
                        organisms_list
                        publish(PrintInLogs("Which organism would you like to consider? (l to list available organisms)"))
                        return "set"
                    } else {
                        publish(PrintInLogs("Which field would you like to set? (SPD, HP, POW, DEF, DEC)"))
                        return (unSplitCommand(splited_command))
                    }
                }
                case 3 => {
                    publish(PrintInLogs("What is the target value ? (integer)"))
                    return (unSplitCommand(splited_command))
                }
                case 4 => {
                    val target_organism = getOrganismById(splited_command(1).toInt)
                    val target_field    = splited_command(2)
                    val target_value    = splited_command(3).toInt
                    val stat: Stat = target_field match {
                        case "SPD" => { target_organism.stats.speed }
                        case "HP" =>  { target_organism.stats.health }
                        case "POW" => { target_organism.stats.power }
                        case "DEF" => { target_organism.stats.resistance }
                        case "DEC" => { target_organism.stats.decisiveness }
                        case _ =>     { publish(PrintInLogs("Error: unknown field `" + target_field + "`")); null }
                    }
                    if(stat != null) {
                        stat.base = target_value
                        stat.syncBase
                    }
                    publish(PrintInLogs(target_organism + ""))
                    last_checked_arg = 0
                    return ""
                }
            }
        }

        def organisms_show: String = {// Shows the stats field of a particular organism.
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                    (true,  "show"                                    ),
                    (true,  "N:0->" + (body.organisms.size - 1) + ";")
                    )
                )) {
                publish(PrintInLogs("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    publish(PrintInLogs("Which organism would you like to see ?"))
                    return "show"
                }
                case 2 => {
                    publish(PrintInLogs("" + getOrganismById(splited_command(1).toInt)))
                    return ""
                }
                case _ => {
                    publish(PrintInLogs("Too many arguments, see `list` to list several organisms"))
                    return ""
                }
            }
        }

        splited_command(0) match {// main switch to defines the function that corresponds to the command at hand.
            case "list"     => { organisms_list; return ""}
            case "set"      => { return organisms_set     }
            case "show"     => { return organisms_show    }
            case _          => { publish(PrintInLogs("Error: Command `" + splited_command(0) + "` unknown")); return "" }
        }
    }
}
