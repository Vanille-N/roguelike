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
        try {
            val src = Source.fromFile("client.cfg")
            val line = src.getLines.next.split(" ")
            val spec = startGame(line(0), line(1).toInt, line(2).toInt)
            val host = spec._1
            val port = spec._2
            val timeout = spec._3
            val socket = validateSocket(host, port, timeout)
            println(s"Listening on $host:$port")
            socket
        } catch {
            case e: java.lang.NumberFormatException => {
                println(s"$e")
                println("When reading configuration file client.cfg")
                println("Provide a valid integer")
                println("Syntax:")
                println("        host port timeout")
                println("e.g.    localhost 8001 20000")
                sys.exit(1)
            }
            case e: java.io.FileNotFoundException => {
                println(s"$e")
                println("Configuration must be provided in client.cfg")
                println("Provide host, port, timeout")
                println("e.g.")
                println("$ cat config.cfg")
                println("localhost 8001 20000")
                sys.exit(1)
            }
            case e: java.lang.ArrayIndexOutOfBoundsException => {
                println(s"$e")
                println("Not enough configuration informations were provided")
                println("Please indicate host, port, and timeout, separated by a single space")
                println("e.g.    localhost 8001 20000")
                sys.exit(1)
            }
        }
    }

	def validateSocket(
        defaultHost: String,
        defaultPort: Int,
        timeout: Int
    ): Socket = {
		var port = defaultPort
		var host = defaultHost
		var connected = false
		var socket = new Socket ()
		val hostRegexp = raw"([0-9]{4}\.){3}[0-9]{4}".r
		val portRegexp = "[0-9]{3,5}".r
		var error = ""

		while (!connected) {
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
										println(s"${host} is a valid host")
									} else error = s"${hostBox.text} is not an IP address"
									if (portRegexp.findAllIn(portBox.text).toList.length == 1) {
										port = portBox.text.toInt
										println(s"${port} is a valid port")
									} else error = s"${portBox.text} is not a port number"
									dialog.close()
								}
								contents += Button("Quit") { sys.exit(0) }
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

	def startGame (
        defHost: String,
        defPort: Int,
        defTimeout: Int
    ): Tuple3[String, Int, Int] = {
		val hostBox = new TextField(defHost, 19)
		val portBox = new TextField(s"$defPort", 5)
		val timeoutBox = new TextField(s"$defTimeout", 5)
		var host: String = defHost
		var port: Int = defPort
		var timeout: Int = defTimeout
		val hostRegexp = new Regex("^([0-9]{1,3}\\.){3}[0-9]{1,3}$")
		val portRegexp = new Regex("^[0-9]{3,5}$")
		val timeoutRegexp = new Regex("^[0-9]{1,10}$")
		val dialog = new Dialog {
			visible = true
			title = "Roguelike: Startup dialog"
			contents = {
				val dialog = this
				new BoxPanel(Orientation.Vertical) {
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
			centerOnScreen()
			}
		while (dialog != null && dialog.visible) {
			Thread.sleep(100)
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
