// The following class Serverdeals with the items management
class ServerItemsCommand (room: Room) extends ServerCommandManager (room) {
	def execute (splited_command_arg: Array[String]): Boolean = {
		publish(PrintInLogs(prompt + unSplitCommand(splited_command_arg)))
		var splited_command: Array[String] = splited_command_arg// necessary as the arguments of a function are non mutable
		def items_item: Boolean = {// main redirection command to defines equivalence in the call of a specific function (eg.: item list <-> item_list)
			splited_command(1) match {
				case "add"=> {
					splited_command = splited_command.tail
					splited_command(0) = "item-add"
					return items_add
				}
				case "rm" => {
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
				case _     => { publish(PrintInLogs("Error: command `" + splited_command(1) + "` unknown")); return true }
			}
		}

		def items_list: Boolean = {// List the current list of items on the board
			var i: Int = 0
			publish(PrintInLogs(s"Currently on the board:\n   ---------   Printing ${room.body.items.size} elements   ---------"))
			for ( o <- room.body.items.toList ) {
				publish(PrintInLogs(i + "-\t" + o))
				i += 1
			}
			publish(PrintInLogs(s"   ---------   End of the printing (${room.body.items.size} elements)   ---------"))
			list_inventory
			return false
		}

		def getItemById (id: Int): Item = {
			val items : List[Item] = room.body.items.toList
			if(id > items.length) { null }
			else {
				var i: Int = 0
				for ( o <- items ) {
					if (i == id) {return o}
					i += 1
				}
			}
			null
		}

		def items_add: Boolean = {// Add an item to the board.
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
			room.body.items += new_itm
			room.body.listenTo(new_itm)
			new_itm.drop
			true
		}

		def items_rm: Boolean = {// Remove an item from the board
			getItemById(splited_command(1).toInt).destroy
			return true
		}

		def items_pickUp: Boolean = {// Force each organism to pick up an item from the board
			room.locs(splited_command(1).toInt, splited_command(2).toInt).organisms(0).foreach ( o =>
				if (room.locs(splited_command(1).toInt, splited_command(2).toInt).items.size > 0) {
					val it = room.locs(splited_command(1).toInt, splited_command(2).toInt).items.head
					if (it.pickUp(o)) {
						o.items += it
						publish(PrintInLogs(s"\nI $o pick up the item, yay !"))
					}
				}
			)
			true
		}

		def list_inventory: Unit = {// List the inventory of the player
			var i: Int = 0
			publish(PrintInLogs(s"Held by user:\n   ---------   Printing ${room.body.player.inventory.size} elements   ---------"))
			for ( o <- room.body.player.inventory.toList ) {
				publish(PrintInLogs(i + "-\t" + o))
				i += 1
			}
			publish(PrintInLogs(s"   ---------   End of the printing (${room.body.player.inventory.size} elements))   ---------"))
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
			val orgas : List[Organism] = room.body.organisms.toList
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
			publish(PrintInLogs(s"   ---   Printing ${room.body.organisms.size} organisms:   ---"))
			var i: Int = 0
			for ( o <- room.body.organisms.toList ) {
				if (o.name != "wall cell") {
					publish(PrintInLogs(i + "-\t" + o))
					i += 1
				}
			}
			publish(PrintInLogs(s"   ---   End of the list (${room.body.organisms.size} organisms).   ---", ln_before = true))
		}

		def items_give (forceUse: Boolean): Boolean = {// Give an item from the inventory to an organism
			val target_item: Item = getItemFromInventoryById(splited_command(1).toInt)
			val target_organism: Organism = getOrganismById(splited_command(2).toInt)
			room.body.player.inventory -= target_item
			target_organism.items += target_item
			if (forceUse) {
				target_item.use(target_organism, target_organism)
			}
			publish(PrintInLogs( "The organism " + splited_command(2) + " received the item " + splited_command(1)))
			return false
			true
		}

		def items_level: Boolean = {
			val target_item : Item = getItemById(splited_command(1).toInt)
			val target_level : Int = splited_command(2).toInt
			publish(PrintInLogs(s"The item ${splited_command(1)} is now level ${splited_command(2)}"))
			true
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
			case _ => { publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown")); return false }
		}
		true
	}
}

