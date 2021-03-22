import scala.collection.mutable.Set
import scala.Tuple2

// The following class is required to select organisms.
class SelectionCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("select", "take", "filter", "flush", "selection-print", "selection")
    help_menus = "select" :: "selection" :: "take" :: "filter" :: "flush" :: Nil

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        var splited_command = splited_command_arg

        publish(HeyPrint(prompt + unSplitCommand(splited_command)))

        def selection_syntax_check (syntax: Array[Tuple2[Boolean, String]]): Boolean = {
            /* If the command does not match its syntax, try add a selection name and try again.
            ** if the modified command still does ot fit its syntax, reject the command, if not,
            ** use the modified command.
            */
            if(command_syntax_check(splited_command, syntax)) return true;
            //if (splited_command.length <= 1)
                return false;
            publish(HeyPrint("Selection name assumed implicit.", ln_after=true, ln_before=true))
            splited_command = (splited_command(0) + splited_command(1) + room.body.selection_current + unSplitCommand(splited_command.tail.tail)).split("\\s+")
            if(command_syntax_check(splited_command, syntax)) return true;
            return false;
        }

        val selection_possibilities: String = {
            var result = s"N:1->;${room.body.selection_names.length}"
            for (i <- 0 to room.body.selection_names.length - 1)
                result += s"|${room.body.selection_names(i)}"
            result
        }

        def selection_new: String = {
            /*
            ** This function allows the user to make a new selection.
            ** If the selection already exists, replace its content by the new content,
            **  if not, create a whole new selection.
            */
            if(!selection_syntax_check(
                Array (
                    (false, "selection"),
                    (false, "new"),
                    (true,  "any"),
                    (true,  "rectangle|rect|circ|circle|N:1->2;"),
                    (false, s"N:1->${room.rows};"),
                    (true,  s"N:1->${room.cols};"),
                    (false, s"N:1->${room.rows};"),
                    (true,  s"N:1->${room.cols};"),
                ))) {
                publish(HeyPrint("The command does not fit its syntax :/\n\tAborting."))
                return "";
            }
            splited_command.length match {
                case 3 => {// new <selection name>
                    publish(HeyPrint(
                        "What kind of selection dou you want to make ?\n\t1 -> rectangle\n\t\t|top left and\n\t\t|bottom right locations asked\n\t2 -> circle\n\t\t|center cell and\n\t\t|a cell on the circle asked"))
                    return unSplitCommand(splited_command)
                }
                case 4 => {// new <selection name> (rectangle|circle)
                    publish(HeyPrint("What is the first cell to mark? (click on it or write \"`i` `j`\" in the command line.)"))
                    return unSplitCommand(splited_command)
                }
                case 6 => {// new <selection name> (rectangle|circle) <coord_i coord_j>
                    publish(HeyPrint("What is the second cell to mark? (click on it or write \"`i` `j`\" in the command line.)"))
                    return unSplitCommand(splited_command)
                }
                case 8 => {// new <selection name> (rectangle|circle) <coord_i coord_j> <coord_i coord_j>
                    val i1: Int = splited_command(4).toInt
                    val j1: Int = splited_command(5).toInt
                    val i2: Int = splited_command(6).toInt
                    val j2: Int = splited_command(7).toInt
                    val selection_index: Int = {
                        if (room.body.selection_names.indexOf(splited_command(2)) < 0) {
                            room.body.selection_names = room.body.selection_names.:+(splited_command(2))
                            room.body.selection_organisms = room.body.selection_organisms.:+(Tuple2(Set[Organism](), Set[Organism]()))
                            room.body.selection_names.indexOf(splited_command(2))
                        } else room.body.selection_names.indexOf(splited_command(2))
                    }
                    room.body.selection_organisms(selection_index) = Tuple2(Set(), Set())
                    splited_command(3) match {
                        case "rect" | "1" | "rectangle" => {
                            for (i <- i1 to i2) {
                                for (j <- j1 to j2) {
                                    room.body.selection_organisms(selection_index)._2 ++= room.locs(i, j).organisms(0)
                                    room.body.selection_organisms(selection_index)._1 ++= room.locs(i, j).organisms(1)
                                }
                            }
                        }
                        case "circ" | "circle" | "2" => {
                            val R2: Int = (j2 - j1)^2 + (i2 - i1)^2
                            val R: Int = scala.math.sqrt(R2).ceil.toInt
                            for (i <- i1 - R to i1 + R) {
                                for (j <- j1 - R to j1 + R) {
                                    if (0 <= i && i < room.rows && 0 <= j && j < room.cols && ((i1 - i )^2 + (j1 - j)^2) <= R2 ) {
                                        room.body.selection_organisms(selection_index)._2 ++= room.locs(i, j).organisms(0)
                                        room.body.selection_organisms(selection_index)._1 ++= room.locs(i, j).organisms(1)
                                    }
                                }
                            }
                        }
                    }
                    publish(HeyPrint(s"Added ${(room.body.selection_organisms(selection_index)._1.size)} viruses."))
                    publish(HeyPrint(s"Added ${(room.body.selection_organisms(selection_index)._2.size)} cells."))
                    splited_command = Array[String] ("selection", "print", splited_command(1))
                    selection_print
                    return ""
                }
            }
        }

        def selection_switch: String = {
            if(!command_syntax_check(splited_command,
                Array (
                    (false, "selection"),
                    (false, "switch"),
                    (true, "any")
                ))) return ""
            if(room.body.selection_names.indexOf(splited_command(1)) < 0) {
                room.body.selection_names.:+(splited_command(2))
                room.body.selection_organisms.:+(Tuple2(Set(), Set()))
            }
            room.body.selection_current = splited_command(1)
            ""
        }

        def selection_print: String = {
            val selection_id: Int = room.body.selection_names.indexOf(room.body.selection_current)
            var i: Int = 0
            try {
                publish(HeyPrint(s"\t*********\tPrinting\t*********"))
                for (o <- room.body.selection_organisms(selection_id)._1)
                    publish(HeyPrint(s"\t${i} -> $o"))
                for (o <- room.body.selection_organisms(selection_id)._2)
                    publish(HeyPrint(s"\t${i} -> $o"))
                publish(HeyPrint(s"\t*********\tEnd of printing\t*********"))
            } catch {case _ : Throwable => publish(HeyPrint("No such selection"))}
            ""
        }

        def selection_list: String = {
            publish(HeyPrint("List of selection names", ln_before=true))
            for (i <- 0 to room.body.selection_names.length - 1) {
                publish(HeyPrint(s"\t_ `${room.body.selection_names(i)}`\n\t\t->${room.body.selection_organisms(i)._1.size} viruses\n\t\t->${room.body.selection_organisms(i)._2.size} cells"))
            }
            publish(HeyPrint(""))
            return ""
        }

        try {
            splited_command(0) match {// main switch to know what function corresponds to the command at hand.
                case "selection"        => {
                    splited_command(1) match {
                        case "new" => return selection_new
                        case "print" => return selection_print
                        case "switch" => return selection_switch
                        case "list" => return selection_list
                        case _ => return ""
                    }
                }
                case _                  => { publish(HeyPrint(s"Error: Command `${splited_command(0)}` unknown")); return "" }
            }
        } catch {case _: Throwable => publish(HeyPrint(s"Error: Command `${unSplitCommand(splited_command)}` failed.")); return "" }
    }
}

