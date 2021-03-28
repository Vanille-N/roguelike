import java.io.File
import java.io.PrintWriter
import scala.io.Source
import MakeItem._

object GameLoader {
    def loadFile (f: String): CompactGame = {
        val src = Source.fromFile(s"assets/$f.save")
        val lines = src.getLines
        val level = lines.next.toInt
        var inventory = new CompactInventory()
        for (line <- lines) {
            val entry = line.split(" ")
            val id = entry(0) match {
                case "ALCOHOL" => ALCOHOL
                case "MOVE" => MOVE
                case "JAVEL" => JAVEL
                case "HEAT" => HEAT
                case "SPIKE" => SPIKE
                case "LEAK" => LEAK
                case "MEMBRANE" => MEMBRANE
            }
            inventory.contents(id) = entry(1).toInt
        }
        new CompactGame(level, inventory)
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
        out.write(s"${game.level}\n")
        for (itm <- game.inventory.contents) {
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
) {}
