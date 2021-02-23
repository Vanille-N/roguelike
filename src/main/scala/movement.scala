import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.swing._
import javax.swing.BorderFactory._
import java.awt.Font
import java.lang.System
import event._

import Direction._

/* Abstraction for movement and pathfinding
 * - cursor movement (controlled by player)
 * - behavior of organisms moving towards/away from the cursor
 */


class Player (var position: Pos) {
    def placeOnMap (p: Pos) {
        position = p
        p.isFocused = true
    }
    placeOnMap(position)

    def move (dir: Direction): Boolean = {
        val newPosition = position.tryAdd(dir)
        if (newPosition != null) {
            position.isFocused = false
            placeOnMap(newPosition)
            true
        } else false
    }
}

object Behavior extends Enumeration {
    type Behavior = Value
    val SEEK = Value("seek")
    val FLEE = Value("flee")
}
import Behavior._

object PathFinder {
    def next (curr: Pos, focus: Pos, behavior: Behavior): Buffer[Direction] = {
        val possible = Array(STAY, LEFT, RIGHT, DOWN, UP)
        var distances: Buffer[Tuple2[Double, Direction]] = Buffer()
        // all reachable positions
        for (i <- 0 to 4) {
            val newPosition = curr.tryAdd(possible(i))
            if (newPosition != null) distances += Tuple2(newPosition.distanceL2(focus), possible(i))
        }
        // choose best ones
        behavior match {
            case SEEK => distances = distances.sortBy(_._1) // close positions first
            case FLEE => distances = distances.sortBy(- _._1) // far positions first
        }
        distances.map(_._2)
    }
}
