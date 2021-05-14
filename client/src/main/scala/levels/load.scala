import java.io.File
import java.io.PrintWriter
import scala.io.Source
import MakeItem._

object GameLoader {
    def loadFile (f: String): CompactGame = {
        val src = Source.fromFile(s"assets/$f.save")
        val lines = src.getLines
        val level = {
          val vals = lines.next.split(" ")
          assert(vals(0) == "level")
          vals(1).toInt
        }
        var inventory = new CompactInventory()
        var stats = (new DefaultVirusSpawner(null)).stats
        for (line <- lines) {
            val entry = line.split(" ")
            entry(0) match {
                case "item" => {
                    val id = entry(1) match {
                        case "ALCOHOL" => ALCOHOL
                        case "MOVE" => MOVE
                        case "JAVEL" => JAVEL
                        case "HEAT" => HEAT
                        case "SPIKE" => SPIKE
                        case "LEAK" => LEAK
                        case "MEMBRANE" => MEMBRANE
                    }
                    inventory.contents(id) = entry(2).toInt
                }
                case "stat" => {
                    val amount = entry(2).toInt
                    val variability = entry(3).toInt
                    val stat = new StatGen(amount, variability)
                    entry(1) match {
                        case "SPD" => stats.speed = stat
                        case "HTH" => stats.health = stat
                        case "POW" => stats.power = stat
                        case "RES" => stats.resistance = stat
                        case "DEC" => stats.decisiveness = stat
                    }
                }
                case _ => {
                    throw new Exception(s"Unkwown specifier ${entry(0)}")
                }
            }
        }
        new CompactGame(level, inventory, stats)
    }

    def tryLoadFile (f: String): CompactGame = {
        if ((new File(s"assets/$f.save")).exists) {
            loadFile(f)
        } else {
            null
        }
    }

    def saveFile (f: String, game: CompactGame) {
        val out = new PrintWriter(new File(s"assets/$f.save"))
        out.write(s"level ${game.level}\n")
        for (itm <- game.inventory.contents) {
            out.write("item ")
            out.write(itm._1 match {
                case ALCOHOL => "ALCOHOL"
                case MOVE => "MOVE"
                case JAVEL => "JAVEL"
                case HEAT => "HEAT"
                case SPIKE => "SPIKE"
                case LEAK => "LEAK"
                case MEMBRANE => "MEMBRANE"
            })
            out.write(s" ${itm._2}\n")
        }
        def writeStat(name: String, stat: StatGen) {
            out.write(s"stat ${name} ${stat.amount} ${stat.variability}\n")
        }
        writeStat("SPD", game.stats.speed)
        writeStat("HTH", game.stats.health)
        writeStat("POW", game.stats.power)
        writeStat("RES", game.stats.resistance)
        writeStat("DEC", game.stats.decisiveness)
        out.close
    }

    def listSaveFiles: List[String] = {
        val files = new File("assets/")
          .listFiles
          .map(_.getName)
          .filter(_.contains("save"))
          .map(_.split('.')(0))
          .toList
        files.sorted
    }
}

class CompactGame (
    val level: Int,
    val inventory: CompactInventory,
    val stats: StatSetGen,
) {}
