import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set
import scala.collection.mutable.Queue
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

class PathFinder (val envt: Array[Array[Boolean]], val rows: Int, val cols: Int) {
    val distance = Array.ofDim[Array[Array[Int]]](rows, cols)
    for (i <- 0 to rows - 1; j <- 0 to cols - 1) distance(i)(j) = null
    def calcDistances (i: Int, j: Int) {
        // BFS from (i,j)
        val dists = Array.ofDim[Int](rows, cols)
        for (k <- 0 to rows - 1; l <- 0 to cols - 1) dists(k)(l) = -1
        val q = new Queue[Tuple3[Int, Int, Int]]
        q.enqueue(Tuple3(i, j, 0))
        dists(i)(j) = 0
        println("Calculating distances from (" + i + "," + j + ")")
        while (!q.isEmpty) {
            val (k, l, dist) = q.dequeue
            println("Reached (" + k + "," + l + ")")
            for (dk <- -1 to 1; dl <- -1 to 1) {
                if ((dk, dl) != (0, 0)) {
                    val nk = k + dk
                    val nl = l + dl
                    println("Considering (" + nk + "," + nl + ")")
                    if (
                        0 <= nk && nk < rows && 0 <= nl && nl < cols // valid position
                        && dists(nk)(nl) == -1 // not reached yet
                        && (!envt(k)(l) || envt(nk)(nl))
                    ) {
                        dists(nk)(nl) = dist + 1
                        q.enqueue(Tuple3(nk, nl, dist + 1))
                    }
                }
            }
        }
        for (i <- 0 to rows - 1) {
            for (j <- 0 to cols - 1) {
                print(dists(i)(j) + " ")
            }
            println()
        }
        distance(i)(j) = dists
    }
    def getDistance (iCurrent: Int, jCurrent: Int, iTarget: Int, jTarget: Int): Int = {
        // println("Checking (" + iTarget + "," + jTarget + ")")
        if (distance(iTarget)(jTarget) == null) {
            calcDistances(iTarget, jTarget)
        }
        distance(iTarget)(jTarget)(iCurrent)(jCurrent)
    }

    def next (curr: Pos, focus: Pos, behavior: Behavior): Buffer[Direction] = {
        val possible = Array(STAY, LEFT, RIGHT, DOWN, UP)
        var distances: Buffer[Tuple2[Double, Direction]] = Buffer()
        // all reachable positions
        for (i <- 0 to 4) {
            val newPos = curr.tryAdd(possible(i))
            if (newPos != null) distances += Tuple2(getDistance(newPos.i, newPos.j, focus.i, focus.j), possible(i))
        }
        // choose best ones
        behavior match {
            case SEEK => distances = distances.sortBy(_._1) // close positions first
            case FLEE => distances = distances.sortBy(- _._1) // far positions first
        }
        distances.map(_._2)
    }
}
