// The following class Serverdeals with the management of the organisms
class ServerOrganismsCommand (room: Room) extends ServerCommandManager (room) {
	def execute (splited_command: Array[String]): Boolean = {
		publish(PrintInLogs(prompt + unSplitCommand(splited_command)))
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
			publish(PrintInLogs("   ---   Printing " + room.body.organisms.size + " organisms:   ---"))
			var i: Int = 0
			for ( o <- room.body.organisms.toList ) {
				publish(PrintInLogs(i + "-\t" + o))
				i += 1
			}
			publish(PrintInLogs("   ---   End of the list ( " + room.body.organisms.size +"organisms).   ---", ln_before = true))
		}

		def organisms_set: Boolean = {// Allows the user to set a stat field of any organism to any integer value.
			val target_organism = getOrganismById(splited_command(1).toInt)
			val target_field	= splited_command(2)
			val target_value	= splited_command(3).toInt
			val stat: Stat = target_field match {
				case "SPD" => { target_organism.stats.speed }
				case "HP" =>  { target_organism.stats.health }
				case "POW" => { target_organism.stats.power }
				case "DEF" => { target_organism.stats.resistance }
				case "DEC" => { target_organism.stats.decisiveness }
				case _ =>	 { publish(PrintInLogs("Error: unknown field `" + target_field + "`")); null }
			}
			if(stat != null) {
				stat.base = target_value
				stat.syncBase
			}
			publish(PrintInLogs(target_organism + ""))
			return true
		}

		def organisms_show: Boolean = {// Shows the stats field of a particular organism.
			publish(PrintInLogs("" + getOrganismById(splited_command(1).toInt)))
			true
		}

		splited_command(0) match {// main switch to defines the function that corresponds to the command at hand.
			case "list"  => { organisms_list; return true}
			case "set"   => { return organisms_set       }
			case "show"  => { return organisms_show      }
			case _       => { publish(PrintInLogs("Error: Command `" + splited_command(0) + "` unknown")); return true }
		}
	}
}
