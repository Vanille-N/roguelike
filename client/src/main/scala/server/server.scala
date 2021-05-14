import java.net.ServerSocket
import java.io.{ BufferedInputStream, PrintStream, BufferedOutputStream }
import akka.actor._
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import event._

case class FromClientToServer (line: String) extends Event

sealed trait RemoteToLocal
case class MsgRoomInfo(room: LocalRoom) extends RemoteToLocal
case class MsgWinCondition(completion: Int) extends RemoteToLocal
case class MsgClearLogs() extends RemoteToLocal
case class MsgLogText(msg: String) extends RemoteToLocal

sealed trait LocalToRemote
case class AnsCommandRequest(cmd: String) extends LocalToRemote

object ServerTranslator {
    def incoming_toString (msg: RemoteToLocal): String = {
        msg match {
            case MsgRoomInfo(room) => s"ROOM;$room"
            case MsgWinCondition(completion) => s"COMP;$completion"
            case MsgClearLogs() => "CLEAR;"
            case MsgLogText(txt) => if (txt != "") { s"LOG;$txt" } else { "" }
        }
    }

    def incoming_fromString (str: String): RemoteToLocal = {
        val split = str.split(";")
        split(0) match {
            case "ROOM" => MsgRoomInfo(parseRoom(split(1)))
            case "COMP" => MsgWinCondition(split(1).toInt)
            case "CLEAR" => MsgClearLogs()
            case "LOG" => MsgLogText(split(1))
        }
    }

    def parseRoom (str: String): LocalRoom = {
        val split = str.split(",")
        val dim = split(0).split(" ")
        val rows = dim(0).toInt
        val cols = dim(1).toInt
        var room = new LocalRoom(rows, cols)
        for (i <- 0 to rows-1; j <- 0 to cols-1) {
            room.locs(i)(j).fromString(split(i*cols+j + 1))
        }
        room
    }

    def outgoing_toString (msg: LocalToRemote): String = {
        msg match {
            case AnsCommandRequest(cmd) => if (cmd != "") { s"CMD///$cmd" } else { "" }
        }
    }

    def outgoing_fromString (str: String): LocalToRemote = {
        val split = str.split("///")
        split(0) match {
            case "CMD" => AnsCommandRequest(split(1))
        }
    }
}



class Server extends Publisher {
    println("Started server")
	val socket_connection = new ServerSocket (8888)
	val socket = socket_connection.accept()
	val input_stream = new BufferedInputStream(socket.getInputStream())
	val output_stream = new PrintStream(new BufferedOutputStream (socket.getOutputStream()))
	var line: String = ""

	def close: Unit = { socket_connection.close }

	def check_incoming: Unit = {
		if(input_stream.available() < 1) ()
		else {
			// Lecture de l'entrée
			val buffer = new Array[Byte](input_stream.available)
			input_stream.read(buffer)

			// Conversion en string, affichage et renvoi
			line = new String(buffer)
			publish (FromClientToServer(line));
		}
	}

	def send_server (message: String): Unit = {
		output_stream.print(message)
		output_stream.flush()
	}

    def scheduler: Scheduler = ActorSystem.create("timer-example").scheduler
    var runner: Cancellable = null
	runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(1,TimeUnit.SECONDS)) { check_incoming }
}
