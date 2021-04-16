import ArtefactType._
import scala.collection.mutable.Map

// The following class Serverdeals with the artefacts management
class ServerArtefactsCommand (room: Room) extends ServerCommandManager (room) {
	def execute (splited_command_arg: Array[String]): Boolean = {
		publish(PrintInLogs(prompt + unSplitCommand(splited_command_arg)))
		var splited_command: Array[String] = splited_command_arg// necessary as the arguments of a function are non mutable

		def artefacts_artefact: Boolean = {
			splited_command(1) match {
				case "list" => {artefacts_list; return true}
				case "rm" => {
					splited_command = splited_command.tail
					splited_command(0) = "artefact-rm"
					return artefacts_rm
				}
				case "add" => {
					splited_command = splited_command.tail
					splited_command(0) = "artefact-add"
					return artefacts_add
				}
			}
		}

		def artefacts_add: Boolean = {
			val i: Int = splited_command(4).toInt
			val j: Int = splited_command(5).toInt
			val target_level: Int = splited_command(3).toInt
			val artefact_type: ArtefactType = splited_command(2).toInt match {
				case 0 => LEVELUP
				case 1 => LEVELDOWN
				case 2 => LEVELSET
				case 3 => LEVELDOUBLE
				case 4 => LEVELDDOUBLE
			}
			room.locs(i, j).artefacts += {splited_command(1).toInt match {
				case 0 => new Artefact(room.locs(i, j), 5, target_level, artefact_type)
				case 1 => new Murderer(room.locs(i, j), 5, target_level, artefact_type)
				case 2 => new ForceUsage(room.locs(i, j), 5, target_level, artefact_type)
				case 3 => new Temptation(room.locs(i, j), 5, target_level, artefact_type)
				case 4 => new Unattach(room.locs(i, j), 5, target_level, artefact_type)
			}}
			return true
		}

		def artefacts_rm: Boolean = {
			val artefacts: Map[Int, Artefact] = Map()
			var i: Int = 0
			room.locs.map(_.artefacts.foreach(
				a => {
					artefacts += (i -> a)
					i += 1
					}
				))
					artefacts(splited_command(1).toInt).position.artefacts -= artefacts(splited_command(1).toInt)
					publish(PrintInLogs(s"Artefact ${artefacts(splited_command(1).toInt).toString} destroyed"))
					return true
				}

		def artefacts_list: Unit = {
			val artefacts: Map[Int, String] = Map()
			var i: Int = 0
			room.locs.map(_.artefacts.foreach(
				a => {
					artefacts += (i -> a.toString)
					i += 1
					}
				))
			publish(PrintInLogs(s"   ---------   Printing ${artefacts.size} elements   ---------"))
			for (i <- 0 to artefacts.size - 1) publish(PrintInLogs(s"\n\t$i -> ${artefacts(i)}"))
			publish(PrintInLogs(s"   ---------   End of the printing of ${artefacts.size} elements   ---------"))
		}

		splited_command(0) match {// main switch to defines the function which corresponds to the command at hand.
			case "artefact" => { return artefacts_artefact }
			case "artefact-add"  => { return artefacts_add }
			case "artefact-rm"   => { return artefacts_rm }
			case "artefact-list" => { artefacts_list; return true }
			case _ => { publish(PrintInLogs(s"Error: Command `${splited_command(0)}` unknown")); return true }
		}
		return true
	}
}
