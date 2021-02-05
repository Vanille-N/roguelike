import Math._
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.Font
import java.lang.System
import java.util.Random
import event._

class StatGen (var amount: Int, var variability: Int) {
    def instantiate: Stat = {
        var x = amount
        val r = new Random
        x += (variability * r.nextGaussian).round.toInt
        new Stat(x.max(0))
    }
}

class Stat (var amount: Int) {
    def set (new_amount: Int) { amount = new_amount }
    def get: Int = { amount }
    def update (change: Int) { amount += change }
}

class StatSet (
    val speed: Stat,
    val health: Stat,
    val power: Stat,
    val resistance: Stat,
    val decisiveness: Stat,
) {}

class StatSetGen (
    val speed: StatGen,
    val health: StatGen,
    val power: StatGen,
    val resistance: StatGen,
    val decisiveness: StatGen,
) {
    def instantiate: StatSet = {
        new StatSet (
            speed=speed.instantiate,
            health=health.instantiate,
            power=power.instantiate,
            resistance=resistance.instantiate,
            decisiveness=decisiveness.instantiate,
        )
    }
}

class SkillGen (var level: Int) {
    def instantiate: Skill = {
        new Skill(level)
    }
}

class Skill (var level: Int) {
    def set (new_level: Int) { level = new_level }
    def get: Int = { level }
}

class SkillSetGen (
    val blocking: SkillGen = new SkillGen(0),
    val penetration: SkillGen = new SkillGen(0),
    val immunity: SkillGen = new SkillGen(0),
    val power: SkillGen = new SkillGen(0),
) {
    def instantiate: SkillSet = {
        new SkillSet(
            blocking=blocking.instantiate,
            penetration=penetration.instantiate,
            immunity=immunity.instantiate,
            power=power.instantiate,
        )
    }
}

class SkillSet (
    val blocking: Skill,
    val penetration: Skill,
    val immunity: Skill,
    val power: Skill,
) {
    override def toString: String = {
        var s = ""
        if (blocking.get > 0) s += "BLK:" + blocking.get
        if (penetration.get > 0) s += (if (s == "") "" else " ") + "PEN:" + penetration.get
        if (immunity.get > 0) s += (if (s == "") "" else " ") + "IMM:" + immunity.get
        if (power.get > 0) s += (if (s == "") "" else " ") + "POW:" + power.get
        s
    }
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
