import collection.mutable.{ Set, ArrayBuffer }
import concurrent.duration.FiniteDuration
import concurrent.ExecutionContext.Implicits.global

import java.util.concurrent.TimeUnit
import akka.actor._

import swing._
import event._

/* Main game loop
 * - global application layout
 * - user interface
 * - move/battle/spawn coordination
 */

case class LevelClear(player: Player) extends Event
case class LoopStep() extends Event
case class PickedUpKey(o: Organism) extends Event
case class Sacrifice() extends Event

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
 
    var selection_organisms: Array[Set[Organism]] = Array(Set())
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
        syncLocalWithRoom(
            localRoom,
            body.room,
            (player.position.i, player.position.j),
            waitingNotifications.toList,
        )
        val response: ArrayBuffer[RemoteToLocal] = ArrayBuffer(MsgWinCondition(winCondition.completion))
        for (i <- 0 to localRoom.rows-1; j <- 0 to localRoom.cols-1) {
            response += MsgRoomInfo(localRoom.locs(i)(j))
        }
        if (clearLogs) {
            response.append(MsgClearLogs())
        }
        clearLogs = false
        response.append(MsgLogText(logText))
        waitingNotifications.clear
        logText = ""
        response.toList
    }

    // extract relevant information to send over the server
    def syncLocalWithRoom (
        local: LocalRoom,
        room: Room,
        focusPos: Tuple2[Int,Int],
        notifications: List[Tuple2[Int,Int]],
      ) {
        for (i <- 0 to local.rows-1; j <- 0 to local.cols-1) {
            var pos = local.locs(i)(j)
            var src = room.locs(i,j)
            pos.strengthSelf = src.strength(player.id)
            pos.strengthOther = src.strength.sum - src.strength(player.id) - src.strength(0)
            pos.strengthCells = src.strength(0)
            pos.hasFriendlySpawner = src.spawner(player.id) != null
            pos.hasNeutralSpawner = src.spawner.zipWithIndex.filter(x => x._2 != 0 && x._2 != player.id).find(_._1 != null) != None
            pos.hasHostileSpawner = src.spawner(0) != null
            pos.hasArtefacts = (src.artefacts.size != 0)
            pos.hasItems = (src.items.size != 0)
            pos.needsFocus = (pos.i == focusPos._1 && pos.j == focusPos._2)
            pos.hasNotification = false
        }
        for (notif <- notifications) {
            if (notif != null) {
                local.locs(notif._1)(notif._2).hasNotification = true
            }
        }
    }

    def syncStr (data: String): String = {
        val info: List[String] = data.split("\\|\\|\\|").toList.filter(s => s != "" && s != "\n" && s != "OK" )
        val messages = ArrayBuffer[LocalToRemote]()
        for (msg <- info) {
            try {
                messages += ServerTranslator.upload_fromString(msg)
            } catch {
                case e: Throwable => println(s"Warning: received corrupted data <$e>")
            }
        }
        val response = sync(messages.toList)
        response.map(ServerTranslator.download_toString(_)).mkString("|||")
    }

    // what to carry from a level to the next
    def migrateInventory: CompactInventory = {
        (new CompactInventory).compress(player.inventory)
    }
    
    reactions += {
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
                    o.kill(SacrificeKill())
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

import java.util.{Timer,TimerTask}
import java.net.ServerSocket
import io.Source

object main extends App with Reactor with Publisher {
    // read server configuration and accept connections
    val src = Source.fromFile("server.cfg")
    val line = src.getLines.toArray 
    val servers: Array[Connection] = line.zipWithIndex.map(x => {
        val id = x._2 + 1
        val port = x._1.toInt
        val socket_connection = new ServerSocket(port)
        println(s"Waiting for connection on $port")
        val socket = socket_connection.accept()
        new Connection(id, socket)
    })
    println(s"${servers.size} players connected")
    src.close

    // initialize players and games from connections
    var levelNum = 1
    var maxLevelNum = levelNum
    var bodyPart: BodyPart = null
    var players: Array[Player] = servers.map(srv => {
        var pl = new Player(srv.id)
        pl.saveInventory = new CompactInventory()
        pl.startingStats = (new DefaultVirusSpawner(pl)).stats
        pl
    })
    var games = Array[Game]()
    var transfer: Array[String] = players.map(_ => "")
    
    def updateMaxLevel {
        maxLevelNum = levelNum.max(maxLevelNum)
    }

    // instanciate new level
    def makeBodyPart {
        val level = new Level(levelNum, maxLevelNum)
        bodyPart = new BodyPart(level, players.toList)
        listenTo(bodyPart)
        val maxScore = players.map(_.score).max
        games = players.map(pl => {
            pl.placeOnMap(bodyPart.room.locs(10, 10))
            new Game(bodyPart, level.makeWinCondition(bodyPart, pl, players.size, maxScore), pl)
        })
        transfer = players.map(pl => "")
    }
    games.map(g => listenTo(g.winCondition))
    games.map(g => g.command.subCommands.foreach(cmd => listenTo(cmd)))

    var running = false
    val scheduler: Scheduler = ActorSystem.create("game-timer").scheduler
    var runner: Cancellable = null

    // records which clients have responded so that none is left behind
    var clientOk = Array.fill(players.size) { false }
    var clientDisconnected: Set[Int] = Set[Int]()
    var innactivityCounter: Int = 0
    val inactivityTimeout: Int = 50

    servers.map(listenTo(_))
    reactions += {
        case Received(id, s) => {
            println("Received message")
            transfer(id-1) = s
            clientOk(id-1) = true
        }
    }

    // if all clients have responded, advance the computation
    def step {
	clientInnactivityCounter += 1
        if (!running) return
	for(i <- clientDisconnected) clientOk(i) = true
        if (clientOk.find(!_) != None
	    && inactivityCounter < inactivityTimeout) return // Some client is still computing
        println("Step")
        for (i <- 0 to games.size-1) {
            clientOk(i) = false
            val info = games(i).syncStr(transfer(i))
            transfer(i) = ""
            servers(i).send_server(info)
        }
        bodyPart.step
	if (clientInnactivityCounter == inactivityTimeout) {
		for (i <- 0 to games.size-1) {
			if (!clientOk(i)) clientDisconnected = clientDisconnected.+(i)
		}
		if (clientDisconnected.size == games.size) Runtime.getRuntime().halt(0)
	}
	clientInnactivityCounter = 0
    }
    def launchRunner {
        runner = scheduler.schedule(
            FiniteDuration(1, TimeUnit.SECONDS),
            FiniteDuration(50, TimeUnit.MILLISECONDS)
        ) { step }
        running = true
    }

    // setup timers and other initialization for new level
    def loadLevel {
        if (running) {
            println("Cancel")
            runner.cancel
            running = false
        }
        if (bodyPart != null) {
            deafTo(bodyPart)
            games.map(g => deafTo(g.winCondition))
            games.map(g => g.command.subCommands.foreach(cmd => deafTo(cmd)))
            games.map(g => g.player.inventory = g.player.saveInventory.decompress(g.player))
        }
        println(s"Entering level $levelNum")
        makeBodyPart
        servers.map(_.send_server(s"NEWGAME///${bodyPart.room.rows} ${bodyPart.room.cols}"))
        games.map(g => listenTo(g.winCondition))
        games.map(g => g.command.subCommands.foreach(cmd => listenTo(cmd)))
        val scoreboard = "Scoreboard\n" + players.map(pl => s"\t* Player ${pl.id}:   ${pl.score} points").mkString("\n")
        servers.map(srv => srv.send_server(ServerTranslator.download_toString(MsgLogText(s"You are Player ${srv.id}\n"))))
        servers.map(_.send_server(ServerTranslator.download_toString(MsgLogText(scoreboard))))
        launchRunner
    }
    loadLevel
 
    reactions += {
        case LevelClear(player) => {
            games.foreach(g => {
                g.player.score += g.winCondition.completion
                g.player.saveInventory = g.migrateInventory // inventory is only transfered if level is cleared
                g.player.startingStats = g.startingStats.deepCopy // same for boosts
            })
            levelNum += 1
            updateMaxLevel
            loadLevel
        }
        case LevelLoad(k) => {
            if (players.size == 1) {
                levelNum = k
                loadLevel
            }
        }
        case GameLoad(save) => {
            if (players.size == 1) {
                maxLevelNum = save.level
                levelNum = save.level
                games.map(g => g.player.saveInventory = save.inventory)
                games.map(g => g.player.startingStats = save.stats)
                loadLevel
            }
        }
        case GameSave(f) => {
            if (players.size == 1) {
                GameLoader.saveFile(f, new CompactGame(maxLevelNum, players(0).saveInventory, players(0).startingStats.deepCopy))
            }
        }
    }
}

