import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.{ Color, Font }
import java.lang.System
import event._

class Stat (val amount: Int, val variability: Int) {}

class Stats {
    var speed: Stat = new Stat(50, 0)
    var health: Stat = new Stat(50, 0)
    var strength: Stat = new Stat(50, 0)
    var resistance: Stat = new Stat(50, 0)

    def toStrength: Int = 10 // placeholder
}
