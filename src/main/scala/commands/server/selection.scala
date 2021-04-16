import scala.collection.mutable.Set
import scala.Tuple2

// The following class Serveris required to select organisms.
class ServerSelectionCommand (room: Room) extends ServerCommandManager (room) {
	def execute (splited_command_arg: Array[String]): Boolean = {
		var splited_command = splited_command_arg

		publish(PrintInLogs(prompt + unSplitCommand(splited_command)))

		def selection_all: Boolean = {
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
			publish(PrintInLogs(s"Added ${(room.body.selection_organisms(selection_index)._1.size)} viruses."))
			publish(PrintInLogs(s"Added ${(room.body.selection_organisms(selection_index)._2.size)} cells."))
			splited_command = Array[String] ("selection", "print", splited_command(1))
			selection_print
			return true
		}

		def selection_new: Boolean = {
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
			publish(PrintInLogs(s"Added ${(room.body.selection_organisms(selection_index)._1.size)} viruses."))
			publish(PrintInLogs(s"Added ${(room.body.selection_organisms(selection_index)._2.size)} cells."))
			splited_command = Array[String] ("selection", "print", splited_command(1))
			selection_print
			return true
		}

		def selection_switch: Boolean = {
			if(room.body.selection_names.indexOf(splited_command(2)) < 0) {
				room.body.selection_names.:+(splited_command(2))
				room.body.selection_organisms.:+(Tuple2(Set(), Set()))
			}
			room.body.selection_current = splited_command(2)
			return true
		}

		def selection_destroy: Boolean = {
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
			return true
		}

		def selection_print: Boolean = {
			val selection_id: Int = room.body.selection_names.indexOf(room.body.selection_current)
			var i: Int = 0
			try {
				publish(PrintInLogs(s"\t*********\tPrinting ${room.body.selection_current}\t*********"))
				for (o <- room.body.selection_organisms(selection_id)._1)
					publish(PrintInLogs(s"\t${i} -> $o"))
				for (o <- room.body.selection_organisms(selection_id)._2)
					publish(PrintInLogs(s"\t${i} -> $o"))
				publish(PrintInLogs(s"\t*********\tEnd of printing ${room.body.selection_current}\t*********"))
			} catch {case _ : Throwable => publish(PrintInLogs("No such selection"))}
			return true
		}

		def selection_list: Boolean = {
			publish(PrintInLogs("List of selection names", ln_before=true))
			for (i <- 0 to room.body.selection_names.length - 1) {
				publish(PrintInLogs(s"\t_ `${room.body.selection_names(i)}`\n\t\t->${room.body.selection_organisms(i)._1.size} viruses\n\t\t->${room.body.selection_organisms(i)._2.size} cells"))
			}
			publish(PrintInLogs(""))
			return true
		}

		def selection_take: Boolean = {// take every items of the current selected organisms
			val selection_id: Int = room.body.selection_names.indexOf(room.body.selection_current)
			var i: Int = 0
			try {
				for (o <- room.body.selection_organisms(selection_id)._1) {
					room.body.player.inventory ++= o.items
					o.items.empty
				}
			} catch {case _ : Throwable => publish(PrintInLogs("No such selection"))}
			return true
		}

		splited_command(0) match {// main switch to know what function corresponds to the command at hand.
			case "selection"		=> {
				if (splited_command.length == 1) return true
				splited_command(1) match {
					case "all" => return selection_all
					case "new" => return selection_new
					case "print" => return selection_print
					case "switch" => return selection_switch
					case "current" => {publish(PrintInLogs(s"The surrent selection is ${room.body.selection_current}")); return true}
					case "destroy" => return selection_destroy
					case "list" => return selection_list
					case "take" => return selection_take
					case _ => return true
				}
			}
			case _ => { publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown")); return true }
		}
	}
}

