import java.io.File
import scala.io.Source
import MakeItem._

object GameLoader {
    def loadFile (f: String): CompactGame = {
    }

    def tryLoadFile (f: String): CompactGame = {
    }

    def saveFile (f: String, game: CompactGame) {
    }

    def listSaveFiles: List[String] = {
    }
}

class CompactGame (
    val level: Int,
    val inventory: CompactInventory,
) {}
