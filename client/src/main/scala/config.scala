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

object Config {
    def fromFile (file: String): Socket = {
        var spec: Tuple3[String, Int, Int] = null
        try {
            val src = Source.fromFile("client.cfg")
            val line = src.getLines.next.split(" ")
            spec = checkServerConfig(line(0), line(1).toInt, line(2).toInt, "Roguelike -- Config: Choose connection settings")
        } catch {
            case e: Throwable => spec = checkServerConfig("127.0.0.1", 8000, 20000, "Config file not found")
        }
        val host = spec._1
        val port = spec._2
        val timeout = spec._3
        val socket = validateSocket(host, port, timeout)
        println(s"Listening on $host:$port")
        socket
    }

	def validateSocket(
        defaultHost: String,
        defaultPort: Int,
        defaultTimeout: Int
    ): Socket = {
		// Data
		var port = defaultPort
		var host = defaultHost
		var timeout: Int = defaultTimeout

		// Keep track of whether the connexion is extablished or not.
		var connected = false

		// Create a socket
		var socket = new Socket ()

		while (!connected) {
			try {
				socket = new Socket()
				socket.setSoTimeout(timeout)
				socket.connect(new InetSocketAddress(host, port))
				connected = true
			} catch {
				case _: Throwable => {
					val conf = checkServerConfig(host, port, timeout, "Error: unable to connect")
					host = conf._1
					port = conf._2
					timeout = conf._3
				}
			}
		}
		println(s"Connected to $host:$port")
		socket
	}

	def checkServerConfig (
        defHost: String,
        defPort: Int,
        defTimeout: Int,
		message: String
    ): Tuple3[String, Int, Int] = {
		// Graphical elements
		val hostBox = new TextField(defHost, 19)
		val portBox = new TextField(s"$defPort", 5)
		val timeoutBox = new TextField(s"$defTimeout", 10)

		// Global variables
		var host: String = defHost
		var port: Int = defPort
		var timeout: Int = defTimeout

		// Regex to recognize numbers & ip addr
		val hostRegexp = new Regex("^([0-9]{1,3}\\.){3}[0-9]{1,3}$")
		val portRegexp = new Regex("^[0-9]{3,5}$")
		val timeoutRegexp = new Regex("^[0-9]{1,10}$")
		// Actual dialog
		val dialog = new Dialog {
            title = message
			contents = {
				val dialog = this
				new BoxPanel(Orientation.Vertical) {
					contents += new Label (s"$message")
					contents += new Label (s"Default server: $defHost:$defPort")
					contents += hostBox
					contents += portBox
					contents += timeoutBox
					contents += Button("Start the game") { dialog.close() }
					contents += Button("Save preferrences") {
						val file = new File("client.cfg")
						val writer = new PrintWriter(file)
						writer.write(s"$host $port $timeout")
						writer.close()
						println(s"'$host $port $timeout' written")
					}
					contents += Button("Quit")      { sys.exit(0) }
					border = Swing.EmptyBorder(10, 10, 10, 10)
				}
			}
            minimumSize = new Dimension(500, 200)
			centerOnScreen
            visible = true
		}
		while (dialog != null && dialog.visible) {
			Thread.sleep(100)
			// If the expressions fit the expected answers, change the values
			if (hostRegexp.findAllIn(hostBox.text).toList.length == 1)
				host = hostBox.text
			if (portRegexp.findAllIn(portBox.text).toList.length == 1)
				port = portBox.text.toInt
			if (timeoutRegexp.findAllIn(timeoutBox.text).toList.length == 1)
				timeout = timeoutBox.text.toInt
		}
		(host, port, timeout)
	}
}
