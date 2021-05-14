
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
