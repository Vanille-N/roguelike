// WARNING
// This file is symlinked between
//       {client,server}/src/main/scala/server/communication.scala
//
// When modifying it take care not to
// - use classes that one party has no knowledge of
// - redefine classes
//
// This file is and should remain self-contained
// in terms of class definitions

import swing.event._

case class Notification (i: Int, j: Int) extends Event

sealed trait RemoteToLocal
case class MsgRoomInfo(room: LocalRoom) extends RemoteToLocal
case class MsgWinCondition(completion: Int) extends RemoteToLocal
case class MsgClearLogs() extends RemoteToLocal
case class MsgLogText(msg: String) extends RemoteToLocal

sealed trait LocalToRemote
case class AnsCommandRequest(cmd: String) extends LocalToRemote

object ServerTranslator {
    def download_toString (msg: RemoteToLocal): String = {
        msg match {
            case MsgRoomInfo(room) => s"ROOM;$room"
            case MsgWinCondition(completion) => s"COMP;$completion"
            case MsgClearLogs() => "CLEAR;"
            case MsgLogText(txt) => if (txt != "") { s"LOG;$txt" } else { "" }
        }
    }

    def dowload_fromString (str: String): RemoteToLocal = {
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

    def upload_toString (msg: LocalToRemote): String = {
        msg match {
            case AnsCommandRequest(cmd) => if (cmd != "") { s"CMD///$cmd" } else { "" }
        }
    }

    def upload_fromString (str: String): LocalToRemote = {
        val split = str.split("///")
        split(0) match {
            case "CMD" => AnsCommandRequest(split(1))
        }
    }
}



// Minimal information required to transfer the contents
// of a room relevant for display
class LocalRoom (
    val rows: Int,
    val cols: Int,
) {
    val locs = IndexedSeq.tabulate(rows, cols) {
        (i, j) => new LocalPos(i, j)
    }

    // in-place deep clone
    def transfer (other: LocalRoom) {
        if (rows != other.rows || cols != other.cols) {
            // Have yet to receive info from the server
            return
        }
        for (i <- 0 to rows-1; j <- 0 to cols-1) {
            var pos = locs(i)(j)
            var src = other.locs(i)(j)
            pos.strengthSelf = src.strengthSelf
            pos.strengthOther = src.strengthOther
            pos.strengthCells = src.strengthCells
            pos.hasFriendlySpawner = src.hasFriendlySpawner
            pos.hasNeutralSpawner = src.hasNeutralSpawner
            pos.hasHostileSpawner = src.hasHostileSpawner
            pos.hasArtefacts = src.hasArtefacts
            pos.hasItems = src.hasItems
            pos.needsFocus = src.needsFocus
            pos.hasNotification = src.hasNotification
        }
    }

    override def toString: String = {
        var res = s"$rows $cols"
        for (i <- 0 to rows-1; j <- 0 to cols-1) {
            val pos = locs(i)(j)
            res += s",$pos"
        }
        res
    }
}

// Information required for each position
class LocalPos (
    val i: Int,
    val j: Int,
) {
    var strengthSelf = 0
    var strengthOther = 0
    var strengthCells = 0
    var hasFriendlySpawner = false
    var hasNeutralSpawner = false
    var hasHostileSpawner = false
    var hasArtefacts = false
    var hasItems = false

    var needsFocus = false
    var hasNotification = false

    def fromString (str: String) {
        val split = str.split(" ")
        strengthSelf = split(0).toInt
        strengthOther = split(1).toInt
        strengthCells = split(2).toInt
        hasFriendlySpawner = bool(split(3)(0))
        hasNeutralSpawner = bool(split(3)(1))
        hasHostileSpawner = bool(split(3)(2))
        hasArtefacts = bool(split(3)(3))
        hasItems = bool(split(3)(4))
        needsFocus = bool(split(3)(5))
        hasNotification = bool(split(3)(6))
    }

    def bit (b: Boolean): Char = { if (b) '1' else '0' }
    def bool (c: Char): Boolean = { (c == '1') }

    override def toString: String = {
        var res = s"${strengthSelf} ${strengthOther} ${strengthCells}"
        res += s" ${bit(hasFriendlySpawner)}${bit(hasNeutralSpawner)}${bit(hasHostileSpawner)}"
        res += s"${bit(hasArtefacts)}${bit(hasItems)}"
        res += s"${bit(needsFocus)}${bit(hasNotification)}"
        res
    }
}
