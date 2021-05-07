import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.swing._
import event._

/* Main game loop
 * - global application layout
 * - user interface
 * - move/battle/spawn coordination
 */

case class LeftClicked (o: Object) extends Event
case class DisplayContents (i: Int, j: Int) extends Event
case class LevelClear(player: Player) extends Event
case class LoopStep() extends Event
case class PickedUpKey(o: Organism) extends Event
case class Sacrifice() extends Event
case class RefreshDisplay() extends Event

case class LevelLoad(num: Int) extends Event
case class GameLoad(game: CompactGame) extends Event
case class GameSave(file: String) extends Event
case class SaveList() extends Event

/* -- Main environment -- */

class Game (
    body: BodyPart,
    val winCondition: WinCondition,
    val player: Player,
) extends Reactor with Publisher {
    var globalPanel : GridBagPanel = null

    var startingStats = player.startingStats.deepCopy
    var waitingNotifications = ArrayBuffer[Tuple2[Int,Int]]()

    val progressbar = new ProgressBar {
        visible = true
        focusable = false
        background = Scheme.red
        foreground = Scheme.green
    }
    val cmdline = new TextField { // type commands to execute action
        columns = 32
        font = new Font("courier", 0, 17)
        background = Scheme.darkGray
        foreground = Scheme.white
    }
    val logs = new TextArea { // see result of commands and other information
        font = new Font("courier", 0, 15)
        background = Scheme.darkGray
        foreground = Scheme.white
        editable = false
        focusable = false
    }
   
    /* selection_organisms is an array of tuples.
    ** | Each tuple is of thr form:
    ** | | ._1 -> friendly organisms (viruses)
    ** | | ._2 -> non friendly organisms (cells)
    */
    var selection_organisms: Array[Tuple2[Set[Organism],Set[Organism]]] = Array(Tuple2(Set(), Set()))
    var selection_names: Array[String] = Array("_")
    var selection_current: String = "_"

    val localRoom = new LocalRoom(body.room.rows, body.room.cols)
    val displayGrid = new DisplayGrid(localRoom)
    displayGrid.map(listenTo(_))
    body.room.locs.map(listenTo(_))
    localRoom.syncWithRoom(body.room, (player.position.i, player.position.j), List())

    var command = new Command(body, this)
    listenTo(command)
    command.subCommands.foreach(listenTo(_))

    // Set up the elements of the user interface.
    def newGame: GridBagPanel = {
        val grid = new GridPanel(body.room.rows, body.room.cols)
        displayGrid.map(grid.contents += _)

        listenTo(body.room, cmdline);

        val panel = new GridBagPanel {
            def constraints (x: Int, y: Int,
                gridwidth: Int = 1, gridheight: Int = 1,
                weightx: Double = 0.0, weighty: Double = 0.0,
                fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None
            ): Constraints = {
                val c = new Constraints
                c.gridx = x
                c.gridy = y
                c.gridwidth = gridwidth
                c.gridheight = gridheight
                c.weightx = weightx
                c.weighty = weighty
                c.fill = fill
                c
            }
            add(grid, constraints(0, 0, gridheight=3, weightx=0.6, weighty=1.0, fill=GridBagPanel.Fill.Both))
            add(cmdline, constraints(1, 3, weightx=0.4, fill=GridBagPanel.Fill.Horizontal))
            add(progressbar, constraints(0, 3, weightx=0.4, fill=GridBagPanel.Fill.Horizontal))
            add(new ScrollPane(logs) { preferredSize = new Dimension(30, 50) }, constraints(1, 1, weightx=0.3, weighty=1.0, fill=GridBagPanel.Fill.Both))
            add(Button("Close") { sys.exit(0) }, constraints(1, 0, weightx=0.4, fill=GridBagPanel.Fill.Horizontal))
            focusable = true;
        }
        panel.foreground = Scheme.darkGray
        panel.background = Scheme.darkGray
        displayGrid.map(_.updateVisuals)

        listenTo(panel.keys);

        globalPanel = panel

        panel
    }

    def step {
        localRoom.syncWithRoom(body.room, (player.position.i, player.position.j), waitingNotifications.toList)
        waitingNotifications.clear
        displayGrid.map(_.updateVisuals)
        progressbar.value = winCondition.completion
    }

    // what to carry from a level to the next
    def migrateInventory: CompactInventory = {
        (new CompactInventory).compress(player.inventory)
    }
    
    // User clicks on dungeon cell or item button ou type a command
    reactions += {
        case DisplayContents(i, j) => {
            this.cmdline.text += " $i $j"
            command.commandRequest(this.cmdline.text)
        }
        case LeftClicked(o: Object) =>  { globalPanel.requestFocusInWindow() }
        case KeyPressed(_, c, _, _) =>  { synchronized {command.keyPressed(c)} }
        case EditDone(`cmdline`) => { command.commandRequest(this.cmdline.text) }
        case PrintInLogs(str: String, ln_after: Boolean, ln_before: Boolean) => {
            if (ln_after && ln_before) logs.text += "\n" + str + "\n"
            else if (ln_after) logs.text += str + "\n"
            else if (ln_before) logs.text += "\n" + str
            else logs.text += str
        }
        case ClearLogs() => { logs.text = "" }
        case Notification(i:Int, j:Int) => {
            waitingNotifications.append((i,j))
        }
        case RefreshDisplay() => {
            displayGrid.map(_.updateVisuals)
        }
        case Sacrifice() => {
            var points = 0
            for (it <- player.inventory) {
                points += it.sacrificeValue
            }
            player.inventory = Set() // empty inventory
            for (o <- body.organisms) {
                if (o.isFriendly) {
                    points += o.sacrificeValue
                    o.kill(CauseOfDeath.Sacrifice)
                    o.sync
                }
            }
            logs.text += s"\n${points} sacrifice points obtained\n"
            val l = startingStats.sacrificeBoost(points)
            logs.text += s"boosted:"
            logs.text += s"  SPD: ${l(0)}; HP: ${l(1)}; DEC: ${l(4)}\n"
            logs.text += s"  POW: ${l(2)}; DEF: ${l(3)}\n"
        }
    }
    command.commandRequest("help")
    logs.text += winCondition.message
}

import java.util.{Timer,TimerTask}

object main extends SimpleSwingApplication {
    var levelNum = 1
    var maxLevelNum = levelNum
    var bodyPart: BodyPart = null

    var players: List[Player] = List({
        var pl = new Player()
        pl.saveInventory = new CompactInventory()
        pl.startingStats = (new DefaultVirusSpawner(pl)).stats
        pl
    })
    var games: List[Game] = List()

    def updateMaxLevel { maxLevelNum = levelNum.max(maxLevelNum) }
    def makeBodyPart {
        val level = new Level(levelNum, maxLevelNum)
        bodyPart = new BodyPart(level, players)
        games = players.map(pl => {
            pl.placeOnMap(bodyPart.room.locs(10, 10))
            new Game(bodyPart, level.makeWinCondition(bodyPart, pl), pl)
        })
    }

    val top = new MainFrame {
        title = "BodyPart"
        contents = { makeBodyPart; games(0).newGame } 
        centerOnScreen()
    }
    games.map(g => listenTo(g.winCondition))
    games.map(g => g.command.subCommands.foreach(cmd => listenTo(cmd)))
    
    def loadLevel {
        games.map(g => deafTo(g.winCondition))
        games.map(g => g.command.subCommands.foreach(cmd => deafTo(cmd)))
        games.map(g => g.player.inventory = g.player.saveInventory.decompress(g.player))
        println(s"Entering level $levelNum")
        makeBodyPart
        top.contents = games(0).newGame
        games.map(g => listenTo(g.winCondition))
        games.map(g => g.command.subCommands.foreach(cmd => listenTo(cmd)))
        
        val timer = new Timer
        timer.schedule(new TimerTask() {
            def run {
                games(0).globalPanel.requestFocusInWindow
            }
        }, 1)
    }

    reactions += {
        case LevelClear(player) => {
            games.foreach(g => {
                g.player.saveInventory = g.migrateInventory // inventory is only transfered if level is cleared
                g.player.startingStats = g.startingStats.deepCopy // same for boosts
            })
            levelNum += 1
            updateMaxLevel
            loadLevel
        }
        case LevelLoad(k) => {
            levelNum = k
            loadLevel
        }
        case GameLoad(save) => {
            maxLevelNum = save.level
            levelNum = save.level
            games.map(g => g.player.saveInventory = save.inventory)
            games.map(g => g.player.startingStats = save.stats)
            loadLevel
        }
        case GameSave(f) => {
            GameLoader.saveFile(f, new CompactGame(maxLevelNum, players(0).saveInventory, players(0).startingStats.deepCopy))
        }
    }
}
