/* Stat/skills management
 * - creation with random variations
 * - interface
 */

// random variation upon stat creation
class StatGen (var amount: Int, var variability: Int) {
    def instantiate: Stat = {
        new Stat(Rng.gaussian(amount, variability).max(0))
    }
    def deepCopy: StatGen = {
        new StatGen(amount, variability)
    }
}

class Stat (var base: Int) {
    // base is the stat without any (de)buffs
    // current is the actual value, updated at each end of turn
    // residual is the value updated in real time
    var current: Int = base
    var residual: Int = current

    // general model:
    // - cell is initialized with base
    // - permanent (de)buffs update base
    // - temporary (de)buffs update current
    // - at each turn
    //   - residual is set to current
    //   - actions affect residual
    //   - when the turn ends current is set to residual
    // In addition to interaction with boost items, this allows in particular
    // - movement spread over several turns because it only affects speed.residual
    // - cells die only at the end of the turn because being attacked affects health.residual
    def syncBase { current = base; residual = current }
    def syncCurrent { residual = current }
}

// aggregation of stats
class StatSet (
    val speed: Stat,
    val health: Stat,
    val power: Stat,
    val resistance: Stat,
    val decisiveness: Stat,
) {
    def list : List[Stat] = {
        List(speed, health, power, resistance, decisiveness)
    }
    def syncBase {
        speed.syncBase
        health.syncBase
        power.syncBase
        resistance.syncBase
        decisiveness.syncBase
    }
    def syncCurrent {
        speed.syncCurrent
        health.syncCurrent
        power.syncCurrent
        resistance.syncCurrent
        decisiveness.syncCurrent
    }
}

class StatSetGen (
    var speed: StatGen,
    var health: StatGen,
    var power: StatGen,
    var resistance: StatGen,
    var decisiveness: StatGen,
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
    def sacrificeBoost (points: Int): List[Int] = {
        def boost(p: Double): Int = {
            if (Rng.choice(p)) 1 else 0
        }
        var spd = 0
        var hp = 0
        var pow = 0
        var res = 0
        var dec = 0
        for (i <- 1 to points) {
            spd += boost(0.02)
            hp += boost(0.05)
            pow += boost(0.04)
            res += boost(0.03)
            dec += boost(0.01)
        }
        speed.amount += spd
        health.amount += hp
        power.amount += pow
        resistance.amount += res
        decisiveness.amount += res
        List(spd, hp, pow, res, dec)
    }
    def deepCopy: StatSetGen = {
        new StatSetGen(
            speed=speed.deepCopy,
            health=health.deepCopy,
            power=power.deepCopy,
            resistance=resistance.deepCopy,
            decisiveness=decisiveness.deepCopy,
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

// Calculates the current strongest skill level
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
