// The following class deals with the items management
class ItemsCommand (body: BodyPart, game: Game)
extends ClientCommandManager (body, game) {
    val acceptedCommands: List[String] = List("item-add", "item", "item-rm", "item-pickup", "item-level", "item-list", "item-give", "item-use")
    help_menus = "item" :: Nil

    def realExecuteCommand (splited_command_arg: Array[String]): String = {
        publish(PrintInLogs(prompt + unSplitCommand(splited_command_arg)))
        var splited_command: Array[String] = splited_command_arg// necessary as the arguments of a function are non mutable
        def items_item: String = {// main redirection command to defines equivalence in the call of a specific function (eg.: item list <-> item_list)
            splited_command.length match {
                case 1 => { publish(PrintInLogs("Wrong usage of command `item`\n\t`-> check `help item` to find out :)")); return "" }
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
                            return items_give(false)
                        }
                        case "use" => {
                            splited_command = splited_command.tail
                            splited_command(0) = "item-use"
                            return items_give(true)
                        }
                        case _        => { publish(PrintInLogs("Error: command `" + splited_command(1) + "` unknown")); return "" }
                    }
                }
            }
        }

        def items_list: String = {// List the current list of items on the board
            var i: Int = 0
            publish(PrintInLogs(s"Currently on the board:\n   ---------   Printing ${body.items.size} elements   ---------"))
            for ( o <- body.items.toList ) {
                publish(PrintInLogs(i + "-\t" + o))
                i += 1
            }
            publish(PrintInLogs(s"   ---------   End of the printing (${body.items.size} elements)   ---------"))
            list_inventory
            return ""
        }

        def getItemById (id: Int): Item = {
            val items : List[Item] = body.items.toList
            if (id > items.length) { null }
            else items(id)
        }

        def items_add: String = {// Add an item to the board.
            // Check if the command syntax is correct or not:
            if(!command_syntax_check (
                splited_command,
                Array(
                        (true,  "item-add"),
                        (true,  "N:1->8;"),
                        (false, s"N:1->${body.room.rows};"),
                        (true,  s"N:1->${body.room.cols};")
                    )
                )) {
                publish(PrintInLogs("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => { publish(PrintInLogs("What kind of item do you want to add ?\n\t1 -> Knife\n\t2-> Alcohol\n\t3 -> Move\n\t4 -> Javel\n\t5-> heat\n\t6-> spike\n\t7-> leak\n\t8-> membrane")); return "item-add" }
                case 2 => { publish(PrintInLogs("Where do you want to spawn the new item? (click or respond by `i j` in the cmdline).")); return "item-add " + splited_command(1) }
                case 4 => {
                    val i: Int = splited_command(2).toInt
                    val j: Int = splited_command(3).toInt
                    val new_itm = splited_command(1) match {
                        case "1" => new Knife(body.room.locs(i, j))
                        case "2" => new Alcohol(body.room.locs(i, j))
                        case "3" => new BodyMovement(body.room.locs(i, j))
                        case "4" => new Javel(body.room.locs(i, j))
                        case "5" => new Heat(body.room.locs(i, j))
                        case "6" => new Spike(body.room.locs(i, j))
                        case "7" => new CytoplasmLeak(body.room.locs(i, j))
                        case "8" => new MembraneReplacement(body.room.locs(i, j))
                    }
                    body.items += new_itm
                    body.listenTo(new_itm)
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
                        (true, s"l|N:0->${body.items.size - 1};")
                    )
                )) {
                publish(PrintInLogs("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => { publish(PrintInLogs("What item do you want to remove from the game? (l to list them)")); "item-rm" }
                case _ => {
                    if (splited_command(1) == "l") {
                        items_list
                        return "item-rm"
                    } else {
                        getItemById(splited_command(1).toInt).destroy
                        return ""
                    }
                }
            }
        }

        def items_pickUp: String = {// Force each organism to pick up an item from the board
            splited_command.length match {
                case 1 => { publish(PrintInLogs("Which cell is this about? (any one-word answer for all)")); return "item-pickup" }
                case 2 => {
                    for(i <- 0 to body.room.cols - 1) {
                        for(j<- 0 to body.room.rows - 1) {
                            body.room.locs(i, j).organisms(0).foreach ( o =>
                                if (body.room.locs(splited_command(1).toInt, splited_command(2).toInt).items.size > 0) {
                                    val it = body.room.locs(splited_command(1).toInt, splited_command(2).toInt).items.head
                                    if (it.pickUp(o)) {
                                        o.items += it
                                        publish(PrintInLogs(s"\nI $o pick up the item, yay !"))
                                    }
                                }
                            )
                        }
                    }
                }
                case 3 => {
                    body.room.locs(splited_command(1).toInt, splited_command(2).toInt).organisms(0).foreach ( o =>
                        if (body.room.locs(splited_command(1).toInt, splited_command(2).toInt).items.size > 0) {
                            val it = body.room.locs(splited_command(1).toInt, splited_command(2).toInt).items.head
                            if (it.pickUp(o)) {
                                o.items += it
                                publish(PrintInLogs(s"\nI $o pick up the item, yay !"))
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
            publish(PrintInLogs(s"Held by user:\n   ---------   Printing ${game.player.inventory.size} elements   ---------"))
            for ( o <- game.player.inventory.toList ) {
                publish(PrintInLogs(i + "-\t" + o))
                i += 1
            }
            publish(PrintInLogs(s"   ---------   End of the printing (${game.player.inventory.size} elements))   ---------"))
        }

        def getItemFromInventoryById (id: Int): Item = {
            var i: Int = 0
            for ( o <- game.player.inventory.toList ) {
                if(i == id) return o
                i += 1
            }
            return null
        }

        def getOrganismById (id: Int): Organism = {
            val orgas : List[Organism] = body.organisms.toList
            if (id > orgas.length) { null }
            else {
                var i: Int = 0
                for ( o <- orgas ) {
                    if (o.name != "wall cell") {
                        if (i == id) { return o }
                        i += 1
                    }
                }
            }
            null
        }

        def organisms_list: Unit = {
            publish(PrintInLogs(s"   ---   Printing ${body.organisms.size} organisms:   ---"))
            var i: Int = 0
            for ( o <- body.organisms.toList ) {
                if (o.name != "wall cell") {
                    publish(PrintInLogs(i + "-\t" + o))
                    i += 1
                }
            }
            publish(PrintInLogs(s"   ---   End of the list (${body.organisms.size} organisms).   ---", ln_before = true))
        }

        def items_give (forceUse: Boolean): String = {// Give an item from the inventory to an organism
            // Check if the command syntax is correct or not:
            if (!command_syntax_check (
                splited_command,
                Array(
                        (true, "item-give|item-use"),
                        (true, s"l|N:0->${game.player.inventory.size - 1};"),
                        (true, s"l|N:0->${body.organisms.size - 1};")
                    )
                )) {
                publish(PrintInLogs("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    publish(PrintInLogs("Which item of the inventory would you like to give? (`l` to list the items)"))
                    if (forceUse) {
                        return "item-use"
                    } else {
                        return "item-give"
                    }
                }
                case 2 => {
                    if ( splited_command(1) == "l") {
                        list_inventory
                        publish(PrintInLogs("Which item of the inventory would you like to give? (`l` to list the items)"))
                        if (forceUse) {
                            return "item-use"
                        } else {
                            return "item-give"
                        }
                    } else {
                        publish(PrintInLogs("What organism should receive the item? (`l` to list the organisms)"))
                        return unSplitCommand(splited_command)
                    }
                }
                case 3 => {
                    if(splited_command(2) == "l") {
                        organisms_list
                        publish(PrintInLogs("What organism should receive the item? (`l` to list the organisms)"))
                        return (splited_command(0) + " " + splited_command(1))
                    } else {
                        val target_item: Item = getItemFromInventoryById(splited_command(1).toInt)
                        val target_organism: Organism = getOrganismById(splited_command(2).toInt)
                        game.player.inventory -= target_item
                        target_organism.items += target_item
                        if (forceUse) {
                            target_item.use(target_organism, target_organism)
                        }
                        publish(PrintInLogs( "The organism " + splited_command(2) + " received the item " + splited_command(1)))
                        return ""
                    }
                }
                case _ => {
                    publish(PrintInLogs("Too many arguments for the command `items_give`\n\tAborting."))
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
                        (true, s"l|N:0->${body.items.size - 1};"),
                        (true, "N:0->5;")
                    )
                )) {
                publish(PrintInLogs("The command does not fit its syntax :/\n\tAborting."))
                return ""
            }

            // The syntax is correct. Continue.
            splited_command.length match {
                case 1 => {
                    publish(PrintInLogs("Of whose item do you want to whange the level ? (l to list)"))
                    return "item-level"
                }
                case 2 => {
                    if(splited_command(1) == "l") {
                        items_list
                        return "item-level"
                    } else {
                        publish(PrintInLogs("What new level do you want ? (integer from 0 to 5)"))
                        return unSplitCommand(splited_command)
                    }
                }
                case 3 => {
                    val target_item : Item = getItemById(splited_command(1).toInt)
                    val target_level : Int = splited_command(2).toInt
                    publish(PrintInLogs(s"The item ${splited_command(1)} is now level ${splited_command(2)}"))
                }
            }
            return ""
        }

        splited_command(0) match {// main switch to defines the function which corresponds to the command at hand.
            case "item-add" => { return items_add }
            case "item-rm" => { return items_rm }
            case "item-pickup" => { return items_pickUp }
            case "item-level" => { return items_level }
            case "item-list" => { return items_list }
            case "item-give" => { return items_give(false) }
            case "item-use" => { return items_give(true) }
            case "item" => { return items_item }
            case _ => { publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown")); return "" }
        }
        return ""
    }
}

