import scala.collection.mutable.Set
import scala.swing._
import event._
import ArtefactType._

/* Main game loop
 * - global application layout
 * - user interface
 * - move/battle/spawn coordination
 */

case class LeftClicked (o: Object) extends Event
case class DisplayContents (p: Pos) extends Event
case class LevelClear() extends Event
case class LoopStep() extends Event
case class PickedUpKey(o: Organism) extends Event
case class Sacrifice() extends Event

case class LevelLoad(num: Int) extends Event
case class GameLoad(game: CompactGame) extends Event
case class GameSave(file: String) extends Event
case class SaveList() extends Event

/* -- Main environment -- */

class BodyPart(val level: Level, inventory: CompactInventory = new CompactInventory, var startingStats: StatSetGen)
extends Reactor with Publisher {
    var globalPanel : GridBagPanel = null

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
    var organisms: Set[Organism] = Set() // all alive
    var items: Set[Item] = Set() // all existing
    var nb_destroyed_items: Int = 1// 1 avoid a division by 0
    var organismsBarycenter: Array[Pos] = Array(null, null)

    /* selection_organisms is an array of tuples.
    ** | Each tuple is of thr form:
    ** | | ._1 -> friendly organisms (viruses)
    ** | | ._2 -> non friendly organisms (cells)
    */
    var selection_organisms: Array[Tuple2[Set[Organism],Set[Organism]]] = Array(Tuple2(Set(), Set()))
    var selection_names: Array[String] = Array("_")
    var selection_current: String = "_"
    var repeat: Int = 1

    val room = level.makeRoom(this, startingStats.deepCopy)
    val player = new Player(room.locs(10, 10))
    player.inventory = inventory.decompress

    var command = new Command(room)
    
    val winCondition = level.makeWinCondition(this)
    listenTo(command)
    command.subCommands.foreach(listenTo(_))

    var isPlaying: Boolean = false

    // Set up the elements of the user interface.
    def newGame: GridBagPanel = {
        val grid = new GridPanel(room.rows, room.cols)
        room.locs.map(grid.contents += _)

        listenTo(room, cmdline);

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
        room.locs.map(_.updateVisuals)

        listenTo(panel.keys);

        globalPanel = panel

        panel
    }

    // main loop
    def step {
        organisms.foreach(o => o.stats.syncCurrent)
        val () = { // update barycenter
            var count: Array[Int] = Array(0, 0)
            val i = Array(0, 0)
            val j = Array(0, 0)
            for (o <- organisms) {
                val idx = if (o.isFriendly) 1 else 0
                i(idx) += o.position.i
                j(idx) += o.position.j
                count(idx) += 1
            }
            for (idx <- 0 to 1) {
                organismsBarycenter(idx) = room.locs(i(idx) / count(idx).max(1), j(idx) / count(idx).max(1))
            }
        }
        var active = true
        // loop until no one can move anymore
        while (active) {
            active = false
            organisms.foreach(o => active = o.step(room) || active)
            room.locs.map(_.battle)
        }
        // items progress
        room.locs.map(_.artefacts.foreach(_.step))
        items.foreach(_.step)
        // viruses age
        // + synchronize effects of items and damage
        // + remove all organisms that should die
        organisms.foreach(o => {
            if (o.isFriendly && Rng.choice(0.07)) o.inflictDamage(1, CauseOfDeath.OldAge)
            o.sync
        })
        room.locs.map(_.trySpawn(organisms.size)) // sometimes spawn new organisms
        room.locs.map(_.updateVisuals) // update display

        publish(LoopStep())
        progressbar.value = winCondition.completion
    }

    // what to carry from a level to the next
    def migrateInventory: CompactInventory = {
        (new CompactInventory).compress(player.inventory)
    }
    
    // User clicks on dungeon cell or item button ou type a command
    reactions += {
        case DisplayContents(p: Pos) => { command.locsClicked(p); command.commandRequest(this.cmdline.text) }
        case LeftClicked(o: Object) =>  { globalPanel.requestFocusInWindow() }
        case KeyPressed(_, c, _, _) =>  { synchronized {command.keyPressed(c)} }
        case EditDone(`cmdline`) => { command.commandRequest(this.cmdline.text) }
        case PickedUpItem(i, o) => { if (i.isInstanceOf[Key]) publish(PickedUpKey(o)) }
        case PrintInLogs(str: String, ln_after: Boolean, ln_before: Boolean) => {
            if (ln_after && ln_before) logs.text += "\n" + str + "\n"
            else if (ln_after) logs.text += str + "\n"
            else if (ln_before) logs.text += "\n" + str
            else logs.text += str
        }
        case ClearLogs() => { logs.text = "" }
        case Sacrifice() => {
            var points = 0
            for (it <- player.inventory) {
                points += it.sacrificeValue
            }
            player.inventory = Set() // empty inventory
            for (o <- organisms) {
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
            step
        }
        case DyingItem (i: Item) => {
            deafTo(i)
            nb_destroyed_items = nb_destroyed_items + 1;
            def get_artefact_type: ArtefactType = {
                return Rng.uniform(0, 6) match {
                    case 0 => LEVELUP
                    case 1 => LEVELDOWN
                    case 2 => LEVELSET
                    case 3 => LEVELDOUBLE
                    case 4 => LEVELDDOUBLE
                    case 5 => NONE
                }
            }
            if(Rng.choice(items.size / nb_destroyed_items)
                    && i.position != null) {
                Rng.uniform(0, 5) match {
                    case 0 => i.position.artefacts =
                        i.position.artefacts.+(
                            new Artefact(i.position,
                                Rng.uniform(0, 5),
                                Rng.uniform(0, 5),
                                get_artefact_type))// Artefact
                    case 1 => i.position.artefacts =
                        i.position.artefacts.+(
                            new Murderer(i.position,
                                Rng.uniform(0, 5),
                                Rng.uniform(0, 5),
                                get_artefact_type))// Murderer
                    case 2 => i.position.artefacts =
                        i.position.artefacts.+(
                            new ForceUsage(i.position,
                                Rng.uniform(0, 5),
                                Rng.uniform(0, 5),
                                get_artefact_type))// Force Usage
                    case 3 => i.position.artefacts =
                        i.position.artefacts.+(
                            new Temptation(i.position,
                                Rng.uniform(0, 5),
                                Rng.uniform(0, 5),
                                get_artefact_type))// Temptation
                    case 4 => i.position.artefacts =
                        i.position.artefacts.+(
                            new Unattach(i.position,
                                Rng.uniform(0, 5),
                                Rng.uniform(0, 5),
                                get_artefact_type))// Unattach
                }
            }
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
    var saveInventory = new CompactInventory()
    var startingStats: StatSetGen = (new DefaultVirusSpawner).stats

    def updateMaxLevel { maxLevelNum = levelNum.max(maxLevelNum) }
    def makeBodyPart {
        bodyPart = new BodyPart(new Level(levelNum, maxLevelNum), saveInventory, startingStats.deepCopy)
    }

    val top = new MainFrame {
        title = "BodyPart"
        contents = { makeBodyPart; bodyPart.newGame } 
        centerOnScreen()
    }
    listenTo(bodyPart.winCondition)
    bodyPart.command.subCommands.foreach(listenTo(_))
    
    def loadLevel {
        deafTo(bodyPart.winCondition)
        bodyPart.command.subCommands.foreach(deafTo(_))
        println(s"Entering level $levelNum")
        makeBodyPart
        top.contents = bodyPart.newGame
        listenTo(bodyPart.winCondition)
        bodyPart.command.subCommands.foreach(listenTo(_))
        
        val timer = new Timer
        timer.schedule(new TimerTask() {
            def run {
                bodyPart.globalPanel.requestFocusInWindow
            }
        }, 1)
    }

    reactions += {
        case LevelClear() => {
            saveInventory = bodyPart.migrateInventory // inventory is only transfered if level is cleared
            startingStats = bodyPart.startingStats.deepCopy // same for boosts
            levelNum += 1
            updateMaxLevel
            loadLevel
        }
        case LevelLoad(k) => {
            levelNum = k
            loadLevel
        }
        case GameLoad(g) => {
            maxLevelNum = g.level
            levelNum = g.level
            saveInventory = g.inventory
            startingStats = g.stats
            loadLevel
        }
        case GameSave(f) => {
            GameLoader.saveFile(f, new CompactGame(maxLevelNum, saveInventory, startingStats.deepCopy))
        }
    }
}
