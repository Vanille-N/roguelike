import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import event._
import akka.actor._

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
    var startingStats = player.startingStats.deepCopy
    var waitingNotifications = ArrayBuffer[Tuple2[Int,Int]]()
    var clearLogs: Boolean = false
    var logText: String = ""
 
    /* selection_organisms is an array of tuples.
    ** | Each tuple is of thr form:
    ** | | ._1 -> friendly organisms (viruses)
    ** | | ._2 -> non friendly organisms (cells)
    */
    var selection_organisms: Array[Tuple2[Set[Organism],Set[Organism]]] = Array(Tuple2(Set(), Set()))
    var selection_names: Array[String] = Array("_")
    var selection_current: String = "_"
 
    var command = new Command(body, this)
    listenTo(command)
    command.subCommands.foreach(listenTo(_))
    listenTo(body.room);
    body.room.locs.map(listenTo(_))

    def sync (info: List[LocalToRemote]): List[RemoteToLocal] = {
        for (msg <- info) {
            msg match {
                case AnsCommandRequest(cmd) => {
                    command.commandRequest(cmd)
                }
            }
        }
        val localRoom = new LocalRoom(body.room.rows, body.room.cols)
        localRoom.syncWithRoom(
            body.room,
            (player.position.i, player.position.j),
            waitingNotifications.toList,
        )
        val response = ArrayBuffer(
            MsgRoomInfo(localRoom),
            MsgWinCondition(winCondition.completion)
        )
        if (clearLogs) {
            response.append(MsgClearLogs())
        }
        clearLogs = false
        response.append(MsgLogText(logText))
        waitingNotifications.clear
        logText = ""
        response.toList
    }

    def syncStr (data: String): String = {
        val info: List[LocalToRemote] = data.split("\\|\\|\\|").filter(_ != "").toList.map(ServerTranslator.outgoing_fromString(_))
        val response = sync(info)
        response.map(ServerTranslator.incoming_toString(_)).mkString("|||")
    }

    // what to carry from a level to the next
    def migrateInventory: CompactInventory = {
        (new CompactInventory).compress(player.inventory)
    }
    
    // User clicks on dungeon cell or item button ou type a command
    reactions += {
        //case DisplayContents(i, j) => {
        //    this.cmdline.text += " $i $j"
        //    command.commandRequest(this.cmdline.text)
        //}
        case PrintInLogs(str: String, ln_after: Boolean, ln_before: Boolean) => {
            if (ln_after && ln_before) logText += "\n" + str + "\n"
            else if (ln_after) logText += str + "\n"
            else if (ln_before) logText += "\n" + str
            else logText += str
        }
        case ClearLogs() => {
            clearLogs = true;
            logText = ""
        }
        case Notification(i:Int, j:Int) => {
            waitingNotifications.append((i,j))
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
            logText += s"\n${points} sacrifice points obtained\n"
            val l = startingStats.sacrificeBoost(points)
            logText += s"boosted:"
            logText += s"  SPD: ${l(0)}; HP: ${l(1)}; DEC: ${l(4)}\n"
            logText += s"  POW: ${l(2)}; DEF: ${l(3)}\n"
        }
    }
    command.commandRequest("help")
    logText += winCondition.message
}

class LocalGame (
    rows: Int,
    cols: Int,
) extends Publisher with Reactor {
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
    listenTo(cmdline)
    
    val localRoom = new LocalRoom(rows, cols)
    val displayGrid = new DisplayGrid(localRoom)
    var waitingMsg = ArrayBuffer[LocalToRemote]()
    displayGrid.map(listenTo(_))
    
    // Set up the elements of the user interface.
    def newGame: GridBagPanel = {
        val grid = new GridPanel(rows, cols)
        displayGrid.map(grid.contents += _)


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

    def sync (info: List[RemoteToLocal]): List[LocalToRemote] = {
        for (msg <- info) {
            msg match {
                case MsgRoomInfo(room) => {
                    localRoom.transfer(room)
                    displayGrid.map(_.updateVisuals)
                }
                case MsgWinCondition(compl) => {
                    progressbar.value = compl
                }
                case MsgClearLogs() => {
                    logs.text = ""
                }
                case MsgLogText(txt) => {
                    logs.text += txt
                }
            }
        }
        val response = waitingMsg.toList
        waitingMsg.clear
        response
    }

    def syncStr (data: String): String = {
        val info: List[RemoteToLocal] = data.split("\\|\\|\\|").filter(_ != "").toList.map(ServerTranslator.incoming_fromString(_))
        val response = sync(info)
        response.map(ServerTranslator.outgoing_toString(_)).mkString("|||")
    }

    val bind_keys: Map[Key.Value, String] = Map(// defines the current key-bindings for the app.
        (Key.Up,         "Up"),
        (Key.K,          "Up"),
        (Key.Down,       "Down"),
        (Key.J,          "Down"),
        (Key.Right,      "Right"),
        (Key.L,          "Right"),
        (Key.Left,       "Left"),
        (Key.H,          "Left"),
        (Key.P,          "play"),
        (Key.S,          "stop"),
        (Key.Q,          "quit"),
        (Key.Space,      "toggle"),
        (Key.O,          "list"),
        (Key.N,          "step-multiple"),
        (Key.Enter,      "click-cell")
    )
    def keyPressed(c: Key.Value) {
        c match {
            case Key.Semicolon => cmdline.requestFocusInWindow
            case Key.Colon => cmdline.requestFocusInWindow
            case Key.Escape => globalPanel.requestFocusInWindow
            case _ => {
                try {
                    val k = bind_keys(c)
                    waitingMsg.append(AnsCommandRequest(k))
                } catch {
                    case e: NoSuchElementException => {}
                }
            }
        }
    }


    // User clicks on dungeon cell or item button ou type a command
    reactions += {
        case DisplayContents(i, j) => {
			waitingMsg.append(AnsCommandRequest(s" $i $j"))
			println(s"Touche $i $j enfoncée")
        }
        case LeftClicked(o: Object) =>  { globalPanel.requestFocusInWindow() }
        case KeyPressed(_, c, _, _) =>  { synchronized { keyPressed(c) } }
        case EditDone(`cmdline`) => {
            if (cmdline.text == "q") {
                globalPanel.requestFocusInWindow()
            } else {
                waitingMsg.append(AnsCommandRequest(cmdline.text));
            }
            cmdline.text = ""
        }
        case RefreshDisplay() => {
            displayGrid.map(_.updateVisuals)
        }
    }
}

import java.util.{Timer,TimerTask}

object main extends SimpleSwingApplication with Publisher {
    var transfer = ""
    var local: LocalGame = null

    def makeLocalGame (data: String) {
        val split = data.split(" ")
        val rows = split(0).toInt
        val cols = split(1).toInt
        local = new LocalGame(rows, cols)
        transfer = ""
    }

    val top = new MainFrame {
        title = "BodyPart"
        contents = { new GridBagPanel }
        centerOnScreen()
    }

    val client = new Client(this)
    listenTo(client)
    reactions += {
        case ReceivedFromServer(s) => {
            val data = s.split("///")
            if (data.size > 0 && data(0) == "NEWGAME") {
                makeLocalGame(data(1))
                top.contents = local.newGame

                val timer = new Timer
                timer.schedule(new TimerTask() {
                    def run {
                        local.globalPanel.requestFocusInWindow
                    }
                }, 1)
            } else {
                transfer = local.syncStr(s)
            }
        }
    }
 
    var running = false
    val scheduler: Scheduler = ActorSystem.create("timer").scheduler
    var runner: Cancellable = null
    def step {
        if (!running) return
        publish(SendMessage(transfer))
    }
    def launchRunner {
        runner = scheduler.schedule(
            FiniteDuration(1, TimeUnit.SECONDS),
            FiniteDuration(100, TimeUnit.MILLISECONDS)
        ) { step }
        running = true
    }
    launchRunner
}
