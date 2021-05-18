import io.Source

import java.io.{ FileNotFoundException, IOException }

import Direction._

class HelpCommand (body: BodyPart, game: Game)
extends ClientCommandManager (body, game) {
	// Definition of the first words of a command that are acceptes as artefact
	// commands and help commands that may be usefull
    val acceptedCommands: List[String] = List("help")
    help_menus = Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        var buffer: String = "\n"// Using a buffer is much faster that adding each character at a time to the current logs
        if(splited_command.length == 1) {// the command is: 'help'
            try {
                val src = Source.fromFile("help/help")
                src.foreach { s => buffer += s }
                src.close
                publish(PrintInLogs(buffer))
            } catch {
                case e: FileNotFoundException => println("Error: Help file not found")
                case e: IOException => println("Error: Failed to open help file")
            }
        } else {
			// The command 'help' has arguments => a help file is required for
			// reach of them.
            for (i <- 1 to  splited_command.length - 1) {
                try {
                    publish(PrintInLogs("Reading help from `help/" + splited_command(i) + ".help`", ln_after=true))
                    val src = Source.fromFile("help/" + splited_command(i) + ".help")
                    src.foreach { s => buffer += s }
                    src.close
                    publish(PrintInLogs(buffer))
                } catch {
					// No help file found: an error is sent
					case e: java.io.FileNotFoundException => publish(PrintInLogs("\nInternal Error: help unavailable for `" + splited_command(i) + "`"))
				}
            }
        }
        return ""
    }
}

