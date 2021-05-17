import collection.mutable.{ Set, ArrayBuffer }
import concurrent.duration.FiniteDuration
import concurrent.ExecutionContext.Implicits.global
import io.Source

import java.util.concurrent.TimeUnit
import java.util.{Timer,TimerTask}
import akka.actor._

import scala.util.matching.Regex

import java.net.{ InetSocketAddress, SocketException, Socket }
import java.io.{ File, PrintWriter }

import swing._
import event._

object main extends SimpleSwingApplication with Publisher {
    var incoming = "" // info from server waiting to be handled
    var local: LocalGame = null

    // data: dimensions in format s"$rows $cols"
    // creates a new blank game
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

    val socket = Config.fromFile("client.cfg")

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
        val messages = incoming.split(raw"\|\|\|", -1)
        incoming = messages(messages.size - 1) // last message could be truncated
        var info = ArrayBuffer[RemoteToLocal]()
        // figure out if new game or info on current game
        for (i <- 0 to messages.size-2) {
            val msg = messages(i)
            val data = msg.split("///")
            if (data.size > 0 && data(0) == "NEWGAME") {
                // create a new game and update gui
                println("New Game ", data(1))
                running = false
                makeLocalGame(data(1))
                top.contents = local.newGame

                // wait a bit that the gui is done initializing
                // then request focus and re-launch loop
                val timer = new Timer
                timer.schedule(new TimerTask() {
                    def run {
                        local.globalPanel.requestFocusInWindow
                        running = true
                    }
                }, 1)
                info = ArrayBuffer()
            } else if (msg != "") {
                // normal message, translate then add to queue
                try {
                    info += ServerTranslator.dowload_fromString(msg)
                } catch {
                    case e: Throwable => println(s"Warning: corrupted message\n\t$e\n\t<$msg>")
                }
            }
        }
        client.send_server("OK") // so that the server knows the message was received
        if (local == null) return // means the game is initializing
        // update local copy and prepare answer
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
