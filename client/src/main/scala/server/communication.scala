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
case class MsgRoomInfo(pos: LocalPos) extends RemoteToLocal
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
            case "ROOM" => MsgRoomInfo((new LocalPos).fromString(split(1)))
            case "COMP" => MsgWinCondition(split(1).toInt)
            case "CLEAR" => MsgClearLogs()
            case "LOG" => MsgLogText(split(1))
        }
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
    def transfer (src: LocalPos) {
        if (src.i > rows || src.j > rows) {
            // Have yet to receive info from the server
            return
        }
        var pos = locs(src.i)(src.j)
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
    val i: Int = 0, 
    val j: Int = 0,
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

    def fromString (str: String): LocalPos = {
        val split = str.split(" ")
        var pos = new LocalPos(split(0).toInt, split(1).toInt)
        pos.strengthSelf = split(2).toInt
        pos.strengthOther = split(3).toInt
        pos.strengthCells = split(4).toInt
        pos.hasFriendlySpawner = bool(split(5)(0))
        pos.hasNeutralSpawner = bool(split(5)(1))
        pos.hasHostileSpawner = bool(split(5)(2))
        pos.hasArtefacts = bool(split(5)(3))
        pos.hasItems = bool(split(5)(4))
        pos.needsFocus = bool(split(5)(5))
        pos.hasNotification = bool(split(5)(6))
        pos
    }

    def bit (b: Boolean): Char = { if (b) '1' else '0' }
    def bool (c: Char): Boolean = { (c == '1') }

    override def toString: String = {
        var res = s"$i $j"
        res += s" ${strengthSelf} ${strengthOther} ${strengthCells}"
        res += s" ${bit(hasFriendlySpawner)}${bit(hasNeutralSpawner)}${bit(hasHostileSpawner)}"
        res += s"${bit(hasArtefacts)}${bit(hasItems)}"
        res += s"${bit(needsFocus)}${bit(hasNotification)}"
        res
    }
}
