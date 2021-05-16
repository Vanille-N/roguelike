import concurrent.duration.FiniteDuration
import concurrent.ExecutionContext.Implicits.global
import io.StdIn.readLine

import java.net._
import java.io.{ BufferedInputStream, PrintStream, BufferedOutputStream }
import java.util.concurrent.TimeUnit
import akka.actor._

import swing._
import event._

case class ReceivedFromServer (line: String) extends Event

class Client(host: String, port: Int) extends Publisher {
    println("Connecting to server")
	val socket = new Socket(host, port)
	val in_stream = new BufferedInputStream(socket.getInputStream())
	val out_stream = new PrintStream(new BufferedOutputStream(socket.getOutputStream()))

	def check_incoming {
		if (in_stream.available() < 1) ()
		else {
			val buffer = new Array[Byte](in_stream.available)
			in_stream.read(buffer)
			// Conversion en string
			val line = new String(buffer)

			publish(ReceivedFromServer(line))
            println("Received message")
		}
	}

    def send_server (message: String) {
        out_stream.print(message)
        out_stream.flush()
	}

	def stop {
		out_stream.close()
		socket.close()
	}

    def scheduler: Scheduler = ActorSystem.create("client-timer").scheduler
    var runner: Cancellable = scheduler.schedule(
        FiniteDuration(0,TimeUnit.SECONDS),
        FiniteDuration(50,TimeUnit.MILLISECONDS)
    ) { check_incoming }
}
