import java.net.ServerSocket
import java.io.{ BufferedInputStream, PrintStream, BufferedOutputStream }
import akka.actor._
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import event._

case class FromClientToServer (line: String) extends Event

class Server extends Publisher {
    println("Started server")
	val socket_connection = new ServerSocket (8888)
	val socket = socket_connection.accept()
	val input_stream = new BufferedInputStream(socket.getInputStream())
	val output_stream = new PrintStream(new BufferedOutputStream (socket.getOutputStream()))
	var line: String = ""

	def close: Unit = { socket_connection.close }

	def check_incoming: Unit = {
		if (input_stream.available() < 1) ()
		else {
			// Lecture de l'entrée
			val buffer = new Array[Byte](input_stream.available)
			input_stream.read(buffer)

			// Conversion en string, affichage et renvoi
			line = new String(buffer)
			publish(FromClientToServer(line));
		}
	}

	def send_server (message: String): Unit = {
		output_stream.print(message)
		output_stream.flush()
	}

    def scheduler: Scheduler = ActorSystem.create("server-timer").scheduler
    var runner: Cancellable = null
	runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(100,TimeUnit.MILLISECONDS)) { check_incoming }
}
