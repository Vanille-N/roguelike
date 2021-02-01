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
}

class Skill (val level: Int) {
    def maxVal = 5
}

class Skills {
    var blocking: Skill = new Skill(0)
    var penetration: Skill = new Skill(0)
    var immunity: Skill = new Skill(0)
    var power: Skill = new Skill(0)
}

class SkillRecord {
    var count: Array[Int] = Array(0, 0, 0, 0, 0, 0)
    var level: Int = 0

    def addSkill (s: Skill) = {
        count(s.level) += 1
        if (s.level > level) level = s.level
    }
    def removeSkill (s: Skill) = {
        count(s.level) -= 1
        while (level > 0 && count(level) < 1) level -= 1
    }
}
