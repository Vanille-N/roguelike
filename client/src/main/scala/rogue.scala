import collection.mutable.{ Set, ArrayBuffer }
import concurrent.duration.FiniteDuration
import concurrent.ExecutionContext.Implicits.global

import java.util.concurrent.TimeUnit
import akka.actor._

import swing._
import event._

case class LeftClicked (o: Object) extends Event
case class DisplayContents (i: Int, j: Int) extends Event
case class LoopStep() extends Event
case class RefreshDisplay() extends Event
case class PrintInLogs (str: String, ln_after: Boolean = true, ln_before: Boolean = false) extends Event
case class SendCommandToServer (player_id: Int, command: String) extends Event
case class ClearLogs() extends Event


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
            println(s"<<< $s")
            val data = s.split("///")
            if (data.size > 0 && data(0) == "NEWGAME") {
                println("New Game ", data(1))
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
                println(transfer)
            }
        }
    }
 
    var running = false
    val scheduler: Scheduler = ActorSystem.create("gui-timer").scheduler
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
