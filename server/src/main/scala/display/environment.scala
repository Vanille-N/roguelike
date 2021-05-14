import scala.swing._
import javax.swing.BorderFactory._
import java.awt.Font
import event._

/* Environment: tiles of the "dungeon"
 * - visual feedback for tile contents
 */

case class Notification (i: Int, j: Int) extends Event

class LocalRoom (
    val rows: Int,
    val cols: Int,
) {
    val locs = IndexedSeq.tabulate(rows, cols) {
        (i, j) => new LocalPos(i, j)
    }

    def syncWithRoom (
        room: Room,
        focusPos: Tuple2[Int,Int],
        notifications: List[Tuple2[Int,Int]],
      ) {
        for (i <- 0 to rows-1; j <- 0 to cols-1) {
            var pos = locs(i)(j)
            var src = room.locs(i,j)
            pos.strength(0) = src.strength(0)
            pos.strength(1) = src.strength(1)
            pos.hasFriendlySpawner = (src.friendlySpawner != null)
            pos.hasHostileSpawner = (src.hostileSpawner != null)
            pos.hasArtefacts = (src.artefacts.size != 0)
            pos.hasItems = (src.items.size != 0)
            pos.needsFocus = (pos.i == focusPos._1 && pos.j == focusPos._2)
            pos.hasNotification = false
        }
        for (notif <- notifications) {
            if (notif != null) {
                locs(notif._1)(notif._2).hasNotification = true
            }
        }
    }

    def transfer (other: LocalRoom) {
        if (rows != other.rows || cols != other.cols) {
            // Have yet to receive info from the server
            return
        }
        for (i <- 0 to rows-1; j <- 0 to cols-1) {
            var pos = locs(i)(j)
            var src = other.locs(i)(j)
            pos.strength(0) = src.strength(0)
            pos.strength(1) = src.strength(1)
            pos.hasFriendlySpawner = src.hasFriendlySpawner
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

class LocalPos (
    val i: Int,
    val j: Int,
) {
    var strength = Array(0, 0)
    var hasFriendlySpawner: Boolean = false
    var hasHostileSpawner: Boolean = false
    var hasArtefacts: Boolean = false
    var hasItems: Boolean = false

    var needsFocus: Boolean = false
    var hasNotification: Boolean = false

    def fromString (str: String) {
        val split = str.split(" ")
        strength(0) = split(0).toInt
        strength(1) = split(1).toInt
        hasFriendlySpawner = split(2) == "true"
        hasHostileSpawner = split(3) == "true"
        hasArtefacts = split(4) == "true"
        hasItems = split(5) == "true"
        needsFocus = split(6) == "true"
        hasNotification = split(7) == "true"
    }

    override def toString: String = {
        var res = s"${strength(0)} ${strength(1)}"
        res += s" ${hasHostileSpawner} ${hasFriendlySpawner}"
        res += s" ${hasArtefacts} ${hasItems}"
        res += s" ${needsFocus} ${hasNotification}"
        res
    }


}
