import scala.swing._
import scala.io.Source

/* Room
 * - initialize from text file
 * - create spawners
 */

// The whole room
class Room (val body: BodyPart, fbase: String, players: List[Player])
extends Reactor with Publisher {
    // cell spawners
    val wallSpawner = new DefaultWallCellSpawner()
    val redCellSpawner = new DefaultRedCellSpawner()
    val whiteCellSpawner = new DefaultWhiteCellSpawner()
    val lymphocyteSpawner = new DefaultLymphocyteSpawner()
    val phagocytosisSpawner = new DefaultPhagocytosisSpawner()
    var virusSpawners = players.map(pl => {
        var sp = new DefaultVirusSpawner(pl)
        sp.stats = pl.startingStats
        sp
    })
    val neuronSpawner = new DefaultNeuronSpawner()

    // initialization from src file
    val (rows, cols, locs, pathFinder) = {
        val src = Source.fromFile("assets/" + fbase + ".room") // no error handling because filename is hardcoded
        // first line is dimensions
        val lines = src.getLines
        val dimensions = lines.next.split(" ")
        val rows = dimensions(0).toInt
        val cols = dimensions(1).toInt
        // next is an array of chars
        val locs = new Grid(this, rows, cols)
        val availability = Array.ofDim[Boolean](rows, cols) // give this to the PathFinder
        for (i <- 0 to rows - 1; j <- 0 to cols - 1) availability(i)(j) = true
        for (i <- 0 to rows - 1) {
            val line = lines.next
            for (j <- 0 to cols-1) {
                line(2*j) match {
                    case ' ' => ()
                    case '1' | '2' => locs(i, j).setFriendlySpawner(new PhysicalSpawner(virusSpawners(line(2*j).toInt - '1'.toInt), 0.015, 10))
                    case '#' => { wallSpawner.spawn(locs(i, j)); availability(i)(j) = false }
                    case 'R' => locs(i, j).setHostileSpawner(new PhysicalSpawner(redCellSpawner, 0.03, 7))
                    case 'W' => locs(i, j).setHostileSpawner(new PhysicalSpawner(whiteCellSpawner, 0.02, 5))
                    case 'N' => locs(i, j).setHostileSpawner(new PhysicalSpawner(neuronSpawner, 0.01, 1))
                    case 'P' => locs(i, j).setHostileSpawner(new PhysicalSpawner(phagocytosisSpawner, 0.01, 2))
                    case 'L' => locs(i, j).setHostileSpawner(new PhysicalSpawner(lymphocyteSpawner, 0.02, 8))
                }
            }
        }
        for (i <- 0 to rows - 1; j <- 0 to cols - 1) locs(i, j).forceSpawn // spawners activate at the start
        src.close
        val pathFinder = new PathFinder(availability, rows, cols)
        (rows, cols, locs, pathFinder)
    }

    locs.map(listenTo(_))

    def makeWall (p: Pos, q: Pos) = {
        for (i <- p.i to q.i; j <- p.j to q.j) {
            wallSpawner.spawn(locs(i, j))
        }
    }

    def addOrganism (o: Organism, p: Pos) = {
        o.updateStrength
        o.placeOnMap(p)
        body.organisms.add(o)
    }

    def addItem (i: Item, p: Pos) = {
        body.items.add(i)
        i.setPos(p)
    }

    def isValid (i: Int, j: Int): Boolean = {
        0 <= i && i < rows && 0 <= j && j < cols
    }
}
