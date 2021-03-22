import scala.collection.mutable.Set
import scala.swing._
import event._

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

case class LevelLoad(num: Int) extends Event
case class GameLoad(file: String) extends Event
case class GameSave(file: String) extends Event
case class SaveList() extends Event

/* -- Main environment -- */

class BodyPart(val level: Level, inventory: CompactInventory = new CompactInventory)
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
    var organismsBarycenter: Array[Pos] = Array(null, null)

    var organisms_selection: Array[Set[Organism]] = Array(Set(), Set())
    var repeat: Int = 1

    val room = level.makeRoom(this) // string decides room layout from assets/*.room
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
        /**DEBUG println("next turn") OVER**/
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
            if (o.isFriendly && Rng.choice(0.07)) o.stats.health.residual -= 1
            o.sync
        })
        room.locs.map(_.trySpawn(organisms.size)) // sometimes spawn new organisms
        room.locs.map(_.updateVisuals) // update display

        publish(LoopStep())
        progressbar.value = winCondition.completion
    }

    // what to carry from a level to the next
    def migrate: CompactInventory = {
        (new CompactInventory).compress(player.inventory)
    }

    // User clicks on dungeon cell or item button ou type a command
    reactions += {
        case DisplayContents(p: Pos) => { command.locsClicked(p); command.commandRequest(this.cmdline.text) }
        case LeftClicked(o: Object) =>  { globalPanel.requestFocusInWindow() }
        case KeyPressed(_, c, _, _) =>  { synchronized {command.keyPressed(c)} }
        case EditDone(`cmdline`) => { command.commandRequest(this.cmdline.text) }
        case DyingItem(i: Item) => { logs.text += s"\n * RIP $i, you were a wonderful item *\n"  }
        case NewItem(i: Item) => { logs.text += s"\n * Hi $i, welcome aboard! *\n" }
        case PickedUpItem(i, o) => { if (i.isInstanceOf[Key]) publish(PickedUpKey(o)) }
        case HeyPrint(str: String, ln_after: Boolean, ln_before: Boolean) => {
            if (ln_after && ln_before) logs.text += "\n" + str + "\n"
            else if (ln_after) logs.text += str + "\n"
            else if (ln_before) logs.text += "\n" + str
            else logs.text += str
        }
        case ClearLogs() => { logs.text = "" }
    }
    command.commandRequest("help")
    logs.text += winCondition.message
}

object main extends SimpleSwingApplication {
    var levelNum = 1
    var maxLevelNum = levelNum
    var bodyPart: BodyPart = null
    var saveInventory = new CompactInventory()

    def updateMaxLevel { maxLevelNum = levelNum.max(maxLevelNum) }
    def makeBodyPart {
        bodyPart = new BodyPart(new Level(levelNum, maxLevelNum), saveInventory)
    }

    val top = new MainFrame {
        title = "BodyPart"
        contents = { makeBodyPart; bodyPart.newGame } 
        centerOnScreen()
    }
    listenTo(bodyPart.winCondition)
    
    def nextLevel {
        saveInventory = bodyPart.migrate
        deafTo(bodyPart.winCondition)
        levelNum += 1
        updateMaxLevel
        println("Entering level " + levelNum)
        makeBodyPart
        top.contents = bodyPart.newGame
        listenTo(bodyPart.winCondition)
        // bodyPart.globalPanel.requestFocusInWindow()
    }

    reactions += {
        case LevelClear() => nextLevel
    }
}
