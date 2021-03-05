// The following class is required to select organisms.
class SelectionCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("select", "take", "filter", "flush", "selection-print", "selection")
    help_menus = "select" :: "selection" :: "take" :: "filter" :: "flush" :: Nil

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        appendLogs(prompt + unSplitCommand(splited_command_arg))
        var splited_command: Array[String] = splited_command_arg// necessary because the arguments are non mutable variables
        def selection_select: String = {
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                    (true,  "select"                   ),
                    (true,  "N:1->2;"                  ),
                    (false, "N:1->" + (room.rows) + ";"),
                    (true,  "N:1->" + (room.cols) + ";"),
                    (false, "N:1->" + (room.rows) + ";"),
                    (true,  "N:1->" + (room.cols) + ";")
                    )
                )) {
                appendLogs("The command does not fit its syntax :/\n\tAborting.")
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    appendLogs("What kind of selection do you want to make?\n\t1-> rectangle\n\t\t| the top-left corner or bottom-right corner will be asked\n\t2-> circle\n\t\t| the center and a point on the perimeter will be asked")
                    return "select"
                }
                case 3 | 5 => {
                    appendLogs ("Illegal number of parameters (plausible error: coordinates must be passed as `i j`, not `i`, `j`.)\n\tAborting.")
                    return ""
                }
                case 2 => {
                    appendLogs ("What is the first cell to mark? (click on it or write \"`i` `j`\" in the command line.)")
                    return unSplitCommand(splited_command)
                }
                case 4 => {
                    appendLogs ("What is the second cell to mark? (click on it or write \"`i` `j`\" in the command line.)")
                    return unSplitCommand(splited_command)
                }
                case 6 => {
                    val x1: Int = splited_command(2).toInt
                    val y1: Int = splited_command(3).toInt
                    val x2: Int = splited_command(4).toInt
                    val y2: Int = splited_command(5).toInt
                    room.body.organisms_selection = room.body.organisms_selection.empty
                    splited_command(1) match {
                        case "1" | "rectangle" | "rect" => {// rectangular selection
                            for(i <- x1 to x2) {
                                for (j <- y1 to y2) {
                                    room.body.organisms_selection ++= room.locs(i, j).organisms(0)
                                    room.body.organisms_selection ++= room.locs(i, j).organisms(1)
                                }
                            }
                        }
                        case "2" | "circle" | "circ"       => {// circular selection
                            val R2: Int = (x1 - x2)^2 + (y1 - y2)^2
                            val R: Int = scala.math.sqrt(R2).ceil.toInt
                            for(i <- x1 - R to x1 + R) {
                                for (j <- y1 - R to y1 + R) {
                                    if (0 <= i && i < room.rows && 0 <= j && j < room.cols && ((x1 - i )^2 + (y1 - j)^2) <= R2 ) {
                                        room.body.organisms_selection ++= room.locs(i, j).organisms(0)
                                        room.body.organisms_selection ++= room.locs(i, j).organisms(1)
                                    }
                                }
                            }
                        }
                        case _ => { appendLogs("Internal error: unknown selection type.") }
                    }
                    appendLogs("Selection complete: " + room.body.organisms_selection.size + " elements\n\nUse `selection-print` to print the selection")
                    return ""
                }
                case _ => {
                    appendLogs("Too much arguments... Aborting. :/")
                    return ""
                }
            }
        }

        def selection_take: String = {// take every items of the current selected organisms
            room.body.organisms_selection.foreach(o => {
                if(o.isFriendly) {
                    room.body.player.inventory ++= o.items
                    o.items.empty
                }
            })
            appendLogs("The player has stolen the items of the selected friendly organisms")
            return ""
        }

        def selection_filter: String = {// only keep friendly or unfriendly organisms in the current selection
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                    (true,  "filter"    ),
                    (true,  "virus|cell")
                    )
                )) {
                appendLogs("The command does not fit its syntax :/\n\tAborting.")
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    appendLogs("What family of organism do you want to keep ? (virus|cell)")
                    return "filter"
                }
                case 2 => {
                    if (splited_command(1) == "cell") room.body.organisms_selection = room.body.organisms_selection.filter ( o => !o.isFriendly )
                    else room.body.organisms_selection = room.body.organisms_selection.filter ( o => o.isFriendly )
                    appendLogs("Filter applied to the selection.")
                }
                case _ => { appendLogs("Illegal number of arguments") }
            }
            return ""
        }

        def selection_print: String = {// Prints the current selection
            appendLogs("   ---   Printing " + room.body.organisms_selection.size + " elements:   ---")
            room.body.organisms_selection.foreach ( o => appendLogs("" + o) )
            appendLogs("   ---   End of the selection   ---", ln_before = true)
            return ""
        }

        def selection_selection: String = {// Hub to decide which function is to be executed.
            splited_command.length match {
                case 1 => {
                    appendLogs("Wrong usage of command `selection`\n\t`-> check `help selection` to find out why :)")
                    return ""
                }
                case _ => {
                    splited_command(1) match {
                        case "select" => {
                            splited_command = splited_command.tail
                            return selection_select
                        }
                        case "take"   => return selection_take
                        case "filter" => {
                            splited_command = splited_command.tail
                            return selection_select
                        }
                        case "flush"  => { room.body.organisms_selection --= room.body.organisms_selection; return "" }
                        case "print"  => return selection_print
                        case _        => {
                            appendLogs("Error: command `" + splited_command(1) + "` unknown. Aborting.")
                            return ""
                        }
                    }
                }
            }
            return ""
        }

        splited_command(0) match {// main switch to know what function corresponds to the command at hand.
            case "selection"        => { return selection_selection }
            case "select"           => { return selection_select }
            case "take"             => { return selection_take }
            case "filter"           => { return selection_filter }
            case "flush"            => { room.body.organisms_selection --= room.body.organisms_selection; return "" }
            case "selection-print"  => { return selection_print }
            case _                  => { appendLogs("Error: Command `" + splited_command(0) + "` unknown") }
        }
        return ""
    }
}

