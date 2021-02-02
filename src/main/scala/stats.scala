import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.{ Color, Font }
import java.lang.System
import java.util.Random
import event._

class Stat (val amount: Int, val variability: Int) {
    def instantiate: Int = {
        var x = amount
        val r = new Random
        x += (variability * r.nextGaussian).round.toInt
        x.min(100).max(0)
    }
}

class Stats {
    var speed: Int = 0
    var health: Int = 0
    var power: Int = 0
    var resistance: Int = 0
}

class StatsGenerator {
    var speed: Stat = new Stat(50, 0)
    var health: Stat = new Stat(50, 0)
    var power: Stat = new Stat(50, 0)
    var resistance: Stat = new Stat(50, 0)

    def instantiate: Stats = {
        var s = new Stats
        s.speed = speed.instantiate
        s.health = health.instantiate
        s.power = power.instantiate
        s.resistance = resistance.instantiate
        s
    }
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
