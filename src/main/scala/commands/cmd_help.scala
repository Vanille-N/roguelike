import java.io.FileNotFoundException
import java.io.IOException
import scala.io.Source

import Direction._

class HelpCommand (room: Room) extends CommandManager (room) {
    val acceptedCommands: List[String] = List("help")
    help_menus = Nil

    def realExecuteCommand (splited_command: Array[String]): String = {
        var buffer: String = "\n"// Using a buffer is much faster that adding each character at a time to the current logs
        if(splited_command.length == 1) {
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
            for (i <- 1 to  splited_command.length - 1) {
                try {
                    publish(PrintInLogs("Reading help from `help/" + splited_command(i) + ".help`", ln_after=true))
                    val src = Source.fromFile("help/" + splited_command(i) + ".help")
                    src.foreach { s => buffer += s }
                    src.close
                    publish(PrintInLogs(buffer))
                } catch { case e: java.io.FileNotFoundException => publish(PrintInLogs("\nInternal Error: help unavailable for `" + splited_command(i) + "`")) }
            }
        }
        return ""
    }
}

