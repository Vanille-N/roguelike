// The following class is required to select organisms.
class SelectionCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("select", "take", "filter", "flush", "selection-print", "selection")
    help_menus = "select" :: "selection" :: "take" :: "filter" :: "flush" :: Nil

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        publish(HeyPrint(prompt + unSplitCommand(splited_command_arg)))
        var splited_command: Array[String] = splited_command_arg// necessary because the arguments are non mutable variables
        def selection_select: String = {
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                    (true,  "select"                            ),
                    (true,  "N:1->2;|rectangle|rect|circ|circle|all"),
                    (false, s"N:1->${room.rows};"               ),
                    (true,  s"N:1->${room.cols};"               ),
                    (false, s"N:1->${room.rows};"               ),
                    (true,  s"N:1->${room.cols};"               )
                    )
                )) {
                publish(HeyPrint("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {// command is `select`
                    publish(HeyPrint("What kind of selection do you want to make?\n\t1-> rectangle\n\t\t| the top-left corner or bottom-right corner will be asked\n\t2-> circle\n\t\t| the center and a point on the perimeter will be asked\n\t3-> all"))
                    return "select"
                }
                case 2 => {// command is `select <type>`
                    if (splited_command(1) == "all" || splited_command(1) == "3") {
                        for (i <- 1 to room.rows-1; j <- 1 to room.cols-1) {
                            room.body.organisms_selection(0) ++= room.locs(i, j).organisms(0)
                            room.body.organisms_selection(1) ++= room.locs(i, j).organisms(1)
                        }
                        return ""
                    }
                    publish(HeyPrint("What is the first cell to mark? (click on it or write \"`i` `j`\" in the command line.)"))
                    return unSplitCommand(splited_command)
                }
                case 4 => {// command is `select <type> <coord_i coord_j>`
                    publish(HeyPrint("What is the second cell to mark? (click on it or write \"`i` `j`\" in the command line.)"))
                    return unSplitCommand(splited_command)
                }
                case 6 => {// command is `select <type> <coord_i coord_j> <coord_i coord_j>`
                    val x1: Int = splited_command(2).toInt
                    val y1: Int = splited_command(3).toInt
                    val x2: Int = splited_command(4).toInt
                    val y2: Int = splited_command(5).toInt
                    room.body.organisms_selection(0) = room.body.organisms_selection(0).empty
                    room.body.organisms_selection(1) = room.body.organisms_selection(1).empty
                    splited_command(1) match {
                        case "1" | "rectangle" | "rect" => {// rectangular selection
                            for(i <- x1 to x2) {
                                for (j <- y1 to y2) {
                                    room.body.organisms_selection(0) ++= room.locs(i, j).organisms(0)
                                    room.body.organisms_selection(1) ++= room.locs(i, j).organisms(1)
                                }
                            }
                        }
                        case "2" | "circle" | "circ"       => {// circular selection
                            val R2: Int = (x1 - x2)^2 + (y1 - y2)^2
                            val R: Int = scala.math.sqrt(R2).ceil.toInt
                            for(i <- x1 - R to x1 + R) {
                                for (j <- y1 - R to y1 + R) {
                                    if (0 <= i && i < room.rows && 0 <= j && j < room.cols && ((x1 - i )^2 + (y1 - j)^2) <= R2 ) {
                                        room.body.organisms_selection(0) ++= room.locs(i, j).organisms(0)
                                        room.body.organisms_selection(1) ++= room.locs(i, j).organisms(1)
                                    }
                                }
                            }
                        }
                        case _ => { publish(HeyPrint("Internal error: unknown selection type.")) }
                    }
                    publish(HeyPrint(s"Selection complete: ${room.body.organisms_selection(0).size + room.body.organisms_selection(1).size} elements\n\nUse `selection-print` to print the selection"))
                    return ""
                }
                case _ => {
                    publish(HeyPrint("Too much arguments... Aborting. :/"))
                    return ""
                }
            }
        }

        def selection_take: String = {// take every items of the current selected organisms
            room.body.organisms_selection(0).foreach(o => {
                room.body.player.inventory ++= o.items
                o.items.empty
            })
            publish(HeyPrint("The player has stolen the items of the selected friendly organisms"))
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
                publish(HeyPrint("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    publish(HeyPrint("What family of organism do you want to keep ? (virus|cell)"))
                    return "filter"
                }
                case 2 => {
                    if (splited_command(1) == "cell") room.body.organisms_selection(0) = room.body.organisms_selection(0).empty
                    else room.body.organisms_selection(1) = room.body.organisms_selection(1).empty
                    publish(HeyPrint("Filter applied to the selection."))
                }
                case _ => { publish(HeyPrint("Illegal number of arguments")) }
            }
            return ""
        }

        def selection_print: String = {// Prints the current selection
            publish(HeyPrint(s"   ---   Printing ${room.body.organisms_selection(0).size + room.body.organisms_selection(1).size} elements:   ---"))
            room.body.organisms_selection(0).foreach ( o => publish(HeyPrint("" + o)) )
            room.body.organisms_selection(1).foreach ( o => publish(HeyPrint("" + o)) )
            publish(HeyPrint(s"   ---   End of the selection (${room.body.organisms_selection(0).size + room.body.organisms_selection(1).size} elements)   ---", ln_before = true))
            return ""
        }

        def selection_selection: String = {// Hub to decide which function is to be executed.
            if(!command_syntax_check(
                splited_command,
                Array(
                    (false, "selection"),
                    (true,  "select|take|filter|flush|print"),
                    (true,  "N:1->2;|rectangle|rect|circ|circle|virus|cell"),
                    (false, s"N:1->${room.rows};"                          ),
                    (true,  s"N:1->${room.cols};"                          ),
                    (false, s"N:1->${room.rows};"                          ),
                    (true,  s"N:1->${room.cols};"                          )
                    )
                )) {
                publish(HeyPrint("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }
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
                case "flush"  => {
                    room.body.organisms_selection(1) = room.body.organisms_selection(1).empty
                    room.body.organisms_selection(0) = room.body.organisms_selection(0).empty
                    return "" }
                case "print"  => return selection_print
                case _        => {// unnecessary
                    publish(HeyPrint(s"Error: command `${splited_command(1)}` unknown. Aborting."))
                    return ""
                }
            }
            return ""
        }

        splited_command(0) match {// main switch to know what function corresponds to the command at hand.
            case "selection"        => { return selection_selection }
            case "select"           => { return selection_select }
            case "take"             => { return selection_take }
            case "filter"           => { return selection_filter }
            case "flush"            => {
                room.body.organisms_selection(1) = room.body.organisms_selection(1).empty
                room.body.organisms_selection(0) = room.body.organisms_selection(0).empty
                return ""
            }
            case "selection-print"  => { return selection_print }
            case _                  => { publish(HeyPrint(s"Error: Command `${splited_command(0)}` unknown")) }
        }
        return ""
    }
}

