import java.io.File
import scala.io.Source
import MakeItem._

object GameLoader {
    def loadFile (f: String): CompactGame = {
        println(s"<load $f>")
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
    }

    def saveFile (f: String, game: CompactGame) {
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
