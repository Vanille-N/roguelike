import concurrent.duration.FiniteDuration
import concurrent.ExecutionContext.Implicits.global

import java.net._
import java.io.{ BufferedInputStream, PrintStream, BufferedOutputStream }
import java.util.concurrent.TimeUnit
import akka.actor._

import swing._
import event._

import scala.io.StdIn.readLine
case class SendMessage (line: String) extends Event
case class ReceivedFromServer (line: String) extends Event

class Client (source: Publisher) extends Reactor with Publisher {
    println("Connecting to server")
	val socket: Socket = new Socket("localhost",8888)
	val in_stream: BufferedInputStream = new BufferedInputStream(socket.getInputStream())
	val out_stream = new PrintStream (new BufferedOutputStream(socket.getOutputStream()))
	var line: String = "sdfsdfsd"


	def check_incoming: Unit = {
		if (in_stream.available() < 1) ()
		else {
			val buffer = new Array[Byte](in_stream.available)
			in_stream.read(buffer)
			// Conversion en string
			line = new String(buffer)

			publish (ReceivedFromServer(line))
            println("Received message")
		}
	}

	listenTo (source)

	reactions += {
		case SendMessage (s: String) => {
            println(s">>> $s")
			out_stream.println(line)
			out_stream.flush()
		}
	}

	def stop: Unit = {
		out_stream.close()
		socket.close()
	}

    def scheduler: Scheduler = ActorSystem.create("client-timer").scheduler
    var runner: Cancellable = null
	runner = scheduler.schedule(FiniteDuration(0,TimeUnit.SECONDS), FiniteDuration(100,TimeUnit.MILLISECONDS)) { check_incoming }

}
