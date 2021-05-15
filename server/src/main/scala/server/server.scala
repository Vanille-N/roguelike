import concurrent.duration.FiniteDuration
import concurrent.ExecutionContext.Implicits.global

import java.net.ServerSocket
import java.io.{ BufferedInputStream, PrintStream, BufferedOutputStream }
import java.util.concurrent.TimeUnit
import akka.actor._

import swing._
import event._

case class ReceivedFromClient (line: String) extends Event

class Server extends Publisher {
    println("Started server")
	val socket_connection = new ServerSocket(8888)
	val socket = socket_connection.accept()
	val in_stream = new BufferedInputStream(socket.getInputStream())
	val out_stream = new PrintStream(new BufferedOutputStream(socket.getOutputStream()))

	def close { socket_connection.close }

	def check_incoming {
		if (in_stream.available() < 1) ()
		else {
			// Lecture de l'entrée
			val buffer = new Array[Byte](in_stream.available)
			in_stream.read(buffer)

			// Conversion en string, affichage et renvoi
			val line = new String(buffer)
			publish(ReceivedFromClient(line));
		}
	}

	def send_server (message: String) {
		out_stream.print(message)
		out_stream.flush()
	}

    def scheduler: Scheduler = ActorSystem.create("server-timer").scheduler
    var runner: Cancellable = scheduler.schedule(
        FiniteDuration(0,TimeUnit.SECONDS),
        FiniteDuration(1000,TimeUnit.MILLISECONDS)
    ) { check_incoming }
}
