import collection.mutable.{ Set, ArrayBuffer }
import concurrent.duration.FiniteDuration
import concurrent.ExecutionContext.Implicits.global

import java.util.concurrent.TimeUnit
import akka.actor._

import scala.util.matching.Regex

import java.net.{ InetSocketAddress, SocketException }

import swing._
import event._

case class LeftClicked (o: Object) extends Event
case class DisplayContents (i: Int, j: Int) extends Event
case class LoopStep() extends Event
case class RefreshDisplay() extends Event
case class PrintInLogs (str: String, ln_after: Boolean = true, ln_before: Boolean = false) extends Event
case class SendCommandToServer (player_id: Int, command: String) extends Event
case class ClearLogs() extends Event

// Information known about a game used for visual display
// Updated by means of transfers of LocalRoom
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
    val displayRoom = new DisplayRoom(localRoom)
    var waitingMsg = ArrayBuffer[LocalToRemote]()
    displayRoom.map(listenTo(_))
    
    // Set up the elements of the user interface.
    def newGame: GridBagPanel = {
        val grid = new GridPanel(rows, cols)
        displayRoom.map(grid.contents += _)

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
            focusable = true
        }
        panel.foreground = Scheme.darkGray
        panel.background = Scheme.darkGray
        displayRoom.map(_.updateVisuals)

        listenTo(panel.keys)

        globalPanel = panel

        panel
    }

    def sync (info: List[RemoteToLocal]): List[LocalToRemote] = {
        for (msg <- info) {
            msg match {
                case MsgRoomInfo(pos) => {
                    if (pos.i < displayRoom.rows && pos.j < displayRoom.cols) {
                        localRoom.transfer(pos)
                        displayRoom(pos.i, pos.j).updateVisuals
                    }
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
                waitingMsg.append(AnsCommandRequest(cmdline.text))
            }
            cmdline.text = ""
        }
        case RefreshDisplay() => {
            displayRoom.map(_.updateVisuals)
        }
    }
}

import java.net.Socket
import java.util.{Timer,TimerTask}
import io.Source

object main extends SimpleSwingApplication with Publisher {
    var incoming = ""
    var local: LocalGame = null

    def makeLocalGame (data: String) {
        local = null
        val split = data.split(" ")
        val rows = split(0).toInt
        val cols = split(1).toInt
        local = new LocalGame(rows, cols)
    }

    val top = new MainFrame {
        title = "BodyPart"
        contents = { new GridBagPanel }
        centerOnScreen()
    }

	def getSocket (defaultHost: String,  defaultPort: Int, timeout: Int): Socket = {
		var port: Int = defaultPort
		var host: String = defaultHost
		var connected: Boolean = false
		var socket = new Socket ()
		val hostRegexp = raw"([0-9]{4}\.){3}[0-9]{4}".r
		val portRegexp = "[0-9]{3,5}".r
		var error: String = ""

		while (! connected) {
			try {
				socket = new Socket()
				socket.setSoTimeout(timeout)
				socket.connect(new InetSocketAddress(host, port))
				connected = true
			} catch {
				case _: Throwable => {
					val hostBox = new TextField(host, 19)
					val portBox = new TextField(s"$port", 5)
					val hostRegexp = new Regex("^([0-9]{1,3}\\.){3}[0-9]{1,3}$")
					val portRegexp = new Regex("^[0-9]{3,5}$")
					val dialog = new Dialog {
						visible = true
						title = "Server connection error"
						contents = {
							val dialog = this
							new BoxPanel(Orientation.Vertical) {
								contents += new Label (s"Unable to connect to the server $host:$port ($error)")
								contents += hostBox
								contents += portBox
								contents += Button("Try again") {
									if (hostRegexp.findAllIn(hostBox.text).toList.length == 1) {
										host = hostBox.text
										println(s"${host} is a host: yay !")
									} else error = s"${hostBox.text} is not an IP address"
									if (portRegexp.findAllIn(portBox.text).toList.length == 1) {
										port = portBox.text.toInt
										println(s"${port} is a port: yay !")
									} else error = s"${portBox.text} is not a port number"
									dialog.close()
									}
								contents += Button("Quit")      { Runtime.getRuntime().halt(0) }
								border = Swing.EmptyBorder(10, 10, 10, 10)
							}
						}
						centerOnScreen()
						}
					while (dialog != null && dialog.visible) {
						Thread.sleep(100)
					}
				}
			}
		}
		println(s"Connected to $host:$port")
		socket
	}

    val socket = try {
        val src = Source.fromFile("client.cfg")
        val line = src.getLines.next.split(" ")
        val host = line(0)
        val port = line(1).toInt
        val timeout = line(2).toInt
        val socket = getSocket(host, port, timeout)
        println(s"Listening on $host:$port")
        socket
    } catch {
        case e: java.lang.NumberFormatException => {
            println(s"$e")
            println("When reading configuration file client.cfg")
            println("Provide a valid integer")
            println("Syntax:")
            println("        host port timeout")
            println("e.g.    localhost 8888 2000")
            sys.exit(1)
        }
        case e: java.io.FileNotFoundException => {
            println(s"$e")
            println("Configuration must be provided in client.cfg")
            println("Provide host, port, timeout")
            println("e.g.")
            println("$ cat config.cfg")
            println("localhost 8888 2000")
            sys.exit(1)
        }
        case e: java.lang.ArrayIndexOutOfBoundsException => {
            println(s"$e")
            println("Not enough configuration informations were provided")
            println("Please indicate host, port, and timeout, separated by a single space")
            println("e.g.    localhost 8888 2000")
            sys.exit(1)
        }
    }

    val client = new Connection(0, socket)
    listenTo(client)
    reactions += {
        case Received(_, s) => incoming += s
    }
 
    var running = false
    val scheduler: Scheduler = ActorSystem.create("gui-timer").scheduler
    var runner: Cancellable = null
    def step {
        if (!running) return
        val messages = incoming.split("\\|\\|\\|", -1)
        incoming = messages(messages.size - 1)
        var info = ArrayBuffer[RemoteToLocal]()
        for (i <- 0 to messages.size-2) {
            val msg = messages(i)
            val data = msg.split("///")
            if (data.size > 0 && data(0) == "NEWGAME") {
                println("New Game ", data(1))
                running = false
                makeLocalGame(data(1))
                top.contents = local.newGame

                val timer = new Timer
                timer.schedule(new TimerTask() {
                    def run {
                        local.globalPanel.requestFocusInWindow
                        running = true
                    }
                }, 1)
                info = ArrayBuffer()
            } else if (msg != "") {
                try {
                    info += ServerTranslator.dowload_fromString(msg)
                } catch {
                    case e: Throwable => println(s"Warning: corrupted message\n\t$e\n\t<$msg>")
                }
            }
        }
        client.send_server("OK")
        if (local == null) return // means the game is initializing
        val response = local.sync(info.toList)
        val outgoing = response.map(ServerTranslator.upload_toString(_)).mkString("|||")
        if (outgoing != "") {
            client.send_server(outgoing)
        }
    }
    def launchRunner {
        runner = scheduler.schedule(
            FiniteDuration(1, TimeUnit.SECONDS),
            FiniteDuration(50, TimeUnit.MILLISECONDS)
        ) { step }
        running = true
    }
    launchRunner
}
