import scala.collection.mutable.Set
import scala.Tuple2

// The following class is required to select organisms.
class SelectionCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("select", "take", "filter", "flush", "selection-print", "selection")
    help_menus = "select" :: "selection" :: "take" :: "filter" :: "flush" :: Nil

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        var splited_command = splited_command_arg

        publish(HeyPrint(prompt + unSplitCommand(splited_command)))

        val selection_possibilities: String = {
            var result = s"N:1->;${room.body.selection_names.length}"
            for (i <- 0 to room.body.selection_names.length - 1)
                result += s"|${room.body.selection_names(i)}"
            result
        }

        def selection_all: String = {
            /*
            ** This function allows the user to select all the viruses at once.
            ** If the selection already exists, replace its content by the new
            ** content, if not, create a whole new selection.
            */
            if(!command_syntax_check(
                splited_command,
                Array (// The syntax of this Array is described in
                       // `commands.scala`
                    (false, "selection"),
                    (false, "all"),
                    (true,  "any"),
                ))) {
                publish(HeyPrint("The command does not fit its syntax :/\n\tAborting."))
                return "";
            }
            val selection_index: Int = {
                if (room.body.selection_names.indexOf(splited_command(2)) < 0) {
                    room.body.selection_names = room.body.selection_names.:+(splited_command(2))
                    room.body.selection_organisms = room.body.selection_organisms.:+(Tuple2(Set[Organism](), Set[Organism]()))
                    room.body.selection_names.indexOf(splited_command(2))
                } else room.body.selection_names.indexOf(splited_command(2))
            }
            room.body.selection_organisms(selection_index) = Tuple2(Set(), Set())
            for (i <- 0 to room.rows - 1) {
                for (j <- 0 to room.cols - 1) {
                    room.body.selection_organisms(selection_index)._2 ++= room.locs(i, j).organisms(0)
                    room.body.selection_organisms(selection_index)._1 ++= room.locs(i, j).organisms(1)
                }
            }
            publish(HeyPrint(s"Added ${(room.body.selection_organisms(selection_index)._1.size)} viruses."))
            publish(HeyPrint(s"Added ${(room.body.selection_organisms(selection_index)._2.size)} cells."))
            splited_command = Array[String] ("selection", "print", splited_command(1))
            selection_print
            return ""
        }

        def selection_new: String = {
            /*
            ** This function allows the user to make a new selection.
            ** If the selection already exists, replace its content by the new
            ** content, if not, create a whole new selection.
            */
            if(!command_syntax_check(
                splited_command,
                Array (// The syntax of this Array is described in
                       // `commands.scala`
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
                case 3 => {// selection new <selection name>
                    publish(HeyPrint(
                        "What kind of selection dou you want to make ?\n\t1 -> rectangle\n\t\t|top left and\n\t\t|bottom right locations asked\n\t2 -> circle\n\t\t|center cell and\n\t\t|a cell on the circle asked"))
                    return unSplitCommand(splited_command)
                }
                case 4 => {// new <selection name> (rectangle|circle|rect|circ|1|2)
                    publish(HeyPrint("What is the first cell to mark? (click on it or write \"`i` `j`\" in the command line.)"))
                    return unSplitCommand(splited_command)
                }
                case 6 => {// new <selection name> (rectangle|circle|rect|circ|1|2) <coord_i coord_j>
                    publish(HeyPrint("What is the second cell to mark? (click on it or write \"`i` `j`\" in the command line.)"))
                    return unSplitCommand(splited_command)
                }
                case 8 => {// new <selection name> (rectangle|circle|rect|circ|1|2) <coord_i coord_j> <coord_i coord_j>
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
                Array (// The syntax of this Array is described in
                       // `commands.scala`
                    (false, "selection"),
                    (false, "switch"),
                    (true, "any")
                ))) return ""
            if(room.body.selection_names.indexOf(splited_command(2)) < 0) {
                room.body.selection_names.:+(splited_command(2))
                room.body.selection_organisms.:+(Tuple2(Set(), Set()))
            }
            room.body.selection_current = splited_command(2)
            ""
        }

        def selection_destroy: String = {
            if(!command_syntax_check(splited_command,
                Array (// The syntax of this Array is described in
                       // `commands.scala`
                    (false, "selection"),
                    (false, "destroy"),
                    (true, selection_possibilities)
                ))) return ""
            if(room.body.selection_names.indexOf(splited_command(2)) >= 0) {
                val ind: Int = room.body.selection_names.indexOf(splited_command(2))
                var new_selection_names: Array[String] = Array()
                var new_selection_organisms: Array[Tuple2[Set[Organism], Set[Organism]]] = Array()
                for (i <- room.body.selection_names.indices.filter(_ != ind) ) {
                    new_selection_names = new_selection_names.:+(room.body.selection_names(i))
                    new_selection_organisms = new_selection_organisms.:+(room.body.selection_organisms(i))
                }
                room.body.selection_names = new_selection_names
                room.body.selection_organisms = new_selection_organisms
            }
            ""
        }

        def selection_print: String = {
            val selection_id: Int = room.body.selection_names.indexOf(room.body.selection_current)
            var i: Int = 0
            try {
                publish(HeyPrint(s"\t*********\tPrinting ${room.body.selection_current}\t*********"))
                for (o <- room.body.selection_organisms(selection_id)._1)
                    publish(HeyPrint(s"\t${i} -> $o"))
                for (o <- room.body.selection_organisms(selection_id)._2)
                    publish(HeyPrint(s"\t${i} -> $o"))
                publish(HeyPrint(s"\t*********\tEnd of printing ${room.body.selection_current}\t*********"))
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

        def selection_take: String = {// take every items of the current selected organisms
            val selection_id: Int = room.body.selection_names.indexOf(room.body.selection_current)
            var i: Int = 0
            try {
                for (o <- room.body.selection_organisms(selection_id)._1) {
                    room.body.player.inventory ++= o.items
                    o.items.empty
                }
            } catch {case _ : Throwable => publish(HeyPrint("No such selection"))}
            ""
        }

        splited_command(0) match {// main switch to know what function corresponds to the command at hand.
            case "selection"        => {
                splited_command(1) match {
                    case "all" => return selection_all
                    case "new" => return selection_new
                    case "print" => return selection_print
                    case "switch" => return selection_switch
                    case "current" => {publish(HeyPrint(s"The surrent selection is ${room.body.selection_current}")); return ""}
                    case "destroy" => return selection_destroy
                    case "list" => return selection_list
                    case "take" => return selection_take
                    case _ => return ""
                }
            }
            case _                  => { publish(HeyPrint(s"Error: Command `${splited_command(0)}` unknown")); return "" }
        }
    }
}

