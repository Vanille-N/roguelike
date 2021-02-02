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
