// The following class deals with the items management
class ItemsCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("item-add", "item", "item-rm", "item-pickup", "item-level", "item-list", "item-give")
    help_menus = "item" :: Nil

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        appendLogs(prompt + unSplitCommand(splited_command_arg))
        var splited_command: Array[String] = splited_command_arg// necessary as the arguments of a function are non mutable
        def items_item: String = {// main redirection command to defines equivalence in the call of a specific function (eg.: item list <-> item_list)
            splited_command.length match {
                case 1 => { appendLogs("Wrong usage of command `item`\n\t`-> check `help item` to find out :)"); return "" }
                case _ => {
                    splited_command(1) match {
                        case "add"    => {
                            splited_command = splited_command.tail
                            splited_command(0) = "item-add"
                            return items_add
                        }
                        case "rm"     => {
                            splited_command = splited_command.tail
                            splited_command(0) = "item-rm"
                            return items_rm
                        }
                        case "pickup" =>  {
                            splited_command = splited_command.tail
                            splited_command(0) = "item-pickup"
                            return items_pickUp
                        }
                        case "level"  => {
                            splited_command = splited_command.tail
                            splited_command(0) = "item-level"
                            return items_level
                        }
                        case "list"   => return items_list
                        case "give"   => {
                            splited_command = splited_command.tail
                            splited_command(0) = "item-give"
                            return items_give
                        }
                        case _        => { appendLogs("Error: command `" + splited_command(1) + "` unknown"); return "" }
                    }
                }
            }
        }

        def items_list: String = {// List the current list of items on the board
            var i: Int = 0
            appendLogs(s"   ---------   Printing ${room.body.items.size} elements   ---------")
            for ( o <- room.body.items.toList ) {
                appendLogs(i + "-\t" + o)
                i += 1
            }
            appendLogs(s"   ---------   End of the printing (${room.body.items.size} elements)   ---------")
            return ""
        }

        def getItemById (id: Int): Item = {
            val lily : List[Item] = room.body.items.toList
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

        def items_add: String = {// Add an item to the board.
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                        (true,  "item-add"),
                        (true,  "N:1->8;"),
                        (false, s"N:1->${room.rows};"),
                        (true,  s"N:1->${room.cols};")
                    )
                )) {
                appendLogs("The command does not fit its syntax :/\n\tAborting.")
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => { appendLogs("What kind of item do you want to add ?\n\t1 -> Knife\n\t2-> Alcohol\n\t3 -> Move\n\t4 -> Javel\n\t5-> heat\n\t6-> spike\n\t7-> leak\n\t8-> membrane"); return "item-add" }
                case 2 => { appendLogs("Where do you want to spawn the new item? (click or respond by `i j` in the cmdline)."); return "item-add " + splited_command(1) }
                case 3 => { appendLogs("Syntax error, chack `help` for more details"); return "" }
                case 4 => {
                    val i: Int = splited_command(2).toInt
                    val j: Int = splited_command(3).toInt
                    val new_itm = splited_command(1) match {
                        case "1" => new Knife(room.locs(i, j))
                        case "2" => new Alcohol(room.locs(i, j))
                        case "3" => new BodyMovement(room.locs(i, j))
                        case "4" => new Javel(room.locs(i, j))
                        case "5" => new Heat(room.locs(i, j))
                        case "6" => new Spike(room.locs(i, j))
                        case "7" => new CytoplasmLeak(room.locs(i, j))
                        case "8" => new MembraneReplacement(room.locs(i, j))
                    }
                    new_itm.drop
                    ""
                }
            }
        }

        def items_rm: String = {// Remove an item from the board
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                        (true, "item-rm"),
                        (true, s"l|N:0->${room.body.items.size - 1};")
                    )
                )) {
                appendLogs("The command does not fit its syntax :/\n\tAborting.")
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => { appendLogs("What item do you want to remove from the game? (l to list them)"); "item-rm" }
                case _ => {
                    if (splited_command(1) == "l") {
                        items_list
                        return "item-rm"
                    } else {
                        room.body.items -= getItemById(splited_command(1).toInt)
                        return ""
                    }
                }
            }
        }

        def items_pickUp: String = {// Force each organism to pick up an item from the board
            splited_command.length match {
                case 1 => { appendLogs("Which cell is this about? (any one-word answer for all)"); return "item-pickup" }
                case 2 => {
                    for(i <- 0 to room.cols - 1) {
                        for(j<- 0 to room.rows - 1) {
                            room.locs(i, j).organisms(0).foreach ( o =>
                                if (room.locs(splited_command(1).toInt, splited_command(2).toInt).items.size > 0) {
                                    val it = room.locs(splited_command(1).toInt, splited_command(2).toInt).items.head
                                    if (it.pickUp(o)) {
                                        o.items += it
                                        room.body.logs.text += s"\nI $o pick up the item, yay !"
                                    }
                                }
                            )
                        }
                    }
                }
                case 3 => {
                    room.locs(splited_command(1).toInt, splited_command(2).toInt).organisms(0).foreach ( o =>
                        if (room.locs(splited_command(1).toInt, splited_command(2).toInt).items.size > 0) {
                            val it = room.locs(splited_command(1).toInt, splited_command(2).toInt).items.head
                            if (it.pickUp(o)) {
                                o.items += it
                                room.body.logs.text += s"\nI $o pick up the item, yay !"
                            }
                        }
                    )
                    return ""
                }
            }
            ""
        }

        def list_inventory: Unit = {// List the inventory of the player
            var i: Int = 0
            appendLogs(s"   ---------   Printing ${room.body.player.inventory.size} elements   ---------")
            for ( o <- room.body.player.inventory.toList ) {
                appendLogs(i + "-\t" + o)
                i += 1
            }
            appendLogs(s"   ---------   End of the printing (${room.body.player.inventory.size} elements))   ---------")
        }

        def getItemFromInventoryById (id: Int): Item = {
            var i: Int = 0
            for ( o <- room.body.player.inventory.toList ) {
                if(i == id) return o
                i += 1
            }
            return null
        }

        def getOrganismById (id: Int): Organism = {
            val lily : List[Organism] = room.body.organisms.toList
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
            appendLogs(s"   ---   Printing ${room.body.organisms.size} organisms:   ---")
            var i: Int = 0
            for ( o <- room.body.organisms.toList ) {
                appendLogs(i + "-\t" + o)
                i += 1
            }
            appendLogs(s"   ---   End of the list (${room.body.organisms.size} organisms).   ---", ln_before = true)
        }

        def items_give: String = {// Give an item from the inventory to an organism
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                        (true, "item-give"),
                        (true, s"l|N:0->${room.body.player.inventory.size - 1};"),
                        (true, s"l|N:0->${room.body.organisms.size - 1};")
                    )
                )) {
                appendLogs("The command does not fit its syntax :/\n\tAborting.")
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    appendLogs ("Which item of the inventory would you like to give? (`l` to list the items)")
                    return "item-give"
                }
                case 2 => {
                    if ( splited_command(1) == "l") {
                        list_inventory
                        appendLogs ("Which item of the inventory would you like to give? (`l` to list the items)")
                        return "item-give"
                    } else {
                        appendLogs ("What organism should revieve the item? (`l` to list the organisms)")
                        return unSplitCommand(splited_command)
                    }
                }
                case 3 => {
                    if(splited_command(2) == "l") {
                        organisms_list
                        appendLogs ("What organism should revieve the item? (`l` to list the organisms)")
                        return (splited_command(0) + " " + splited_command(1))
                    } else {
                        val target_item: Item = getItemFromInventoryById(splited_command(1).toInt)
                        val target_organism: Organism = getOrganismById(splited_command(2).toInt)
                        room.body.player.inventory -= target_item
                        target_organism.items += target_item
                        appendLogs ( "The organism " + splited_command(2) + " revieved the item " + splited_command(1))
                        return ""
                    }
                }
                case _ => {
                    appendLogs("Too many arguments for the command `items_give`\n\tAborting.")
                    return ""
                }
            }
        }

        def items_level: String = {
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                        (true, "item-level"),
                        (true, s"l|N:0->${room.body.items.size - 1};"),
                        (true, "N:0->5;")
                    )
                )) {
                appendLogs("The command does not fit its syntax :/\n\tAborting.")
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    appendLogs("Of whose item do you want to whange the level ? (l to list)")
                    return "item-level"
                }
                case 2 => {
                    if(splited_command(1) == "l") {
                        items_list
                        return "item-level"
                    } else {
                        appendLogs("What new level do you want ? (integer from 0 to 5)")
                        return unSplitCommand(splited_command)
                    }
                }
                case 3 => {
                    val target_item : Item = getItemById(splited_command(1).toInt)
                    val target_level : Int = splited_command(2).toInt
                    appendLogs(s"The item ${splited_command(1)} is now level ${splited_command(2)}")
                }
            }
            return ""
        }

        splited_command(0) match {// main switch to defines the function which corresponds to the command at hand.
            case "item-add"    => { return items_add    }
            case "item-rm"     => { return items_rm     }
            case "item-pickup" => { return items_pickUp }
            case "item-level"  => { return items_level  }
            case "item-list"   => { return items_list   }
            case "item-give"   => { return items_give   }
            case "item"        => { return items_item   }
            case _             => { appendLogs(s"Error: Command `${splited_command(0)}` unknown"); return "" }
        }
        return ""
    }
}

